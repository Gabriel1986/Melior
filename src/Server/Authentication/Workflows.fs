module Server.Authentication.Workflows

open System
open Serilog
open Server.Blueprint.Data.Authentication
open Server.Blueprint.Behavior.Storage
open Server.Blueprint.Data.Storage
open Microsoft.IdentityModel.Tokens
open Shared.Library
open Server
open Server.Library
open Server.LibraryExtensions
open Shared.Remoting
open Shared.Write
open Shared.Read

//Should be used internally only!
let addUser (storage: IStorageEngine) (passwordPepper: string) (msg: Message<UserInput>) = async {
    let validated = ValidatedUserInput.Validate passwordPepper msg.Payload
    match validated with
    | Ok validated ->
        let! _ = storage.PersistTransactional [
            validated
            |> UserEvent.UserWasAdded
            |> StorageEvent.UserEvent
            |> inMsg msg
        ]
        return Ok ()
    | Error errors ->
        return Error (CreateUserError.ValidationError errors)
}

let updatePassword (storage: IStorageEngine) (passwordPepper: string) (msg: Message<Guid * string>) = async {
    let passwordHash = Encryption.hashPassword passwordPepper (snd msg.Payload)
    let! nbUsersUpdated = storage.PersistTransactional [
        (fst msg.Payload, passwordHash)
        |> UserEvent.PasswordWasChanged
        |> StorageEvent.UserEvent
        |> inMsg msg        
    ]
    return
        if nbUsersUpdated <> 1
        then Error (exn "Something went terribly wrong while updating the user's password O_o")
        else Ok ()
}

let createUser (storage: IStorageEngine) (msg: Message<User>) = async {
    if msg.CurrentUser.IsSysAdmin () then
        let validated = ValidatedUser.Validate msg.Payload
        //TODO: send email...
        match validated with
        | Ok validated ->
            let! _ = storage.PersistTransactional [
                validated
                |> CUDEvent.Created
                |> UserEvent.UserEvent
                |> StorageEvent.UserEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error errors ->
            return Error (SaveUserError.Validation errors)
    else
        return Error (SaveUserError.AuthorizationError)
}

let updateUser (storage: IStorageEngine) (msg: Message<User>) = async {
    if msg.CurrentUser.IsSysAdmin () then
        let validated = ValidatedUser.Validate msg.Payload
        //TODO: send email...
        match validated with
        | Ok validated ->
            let! nbRowsUpdated = storage.PersistTransactional [
                validated
                |> CUDEvent.Updated
                |> UserEvent.UserEvent
                |> StorageEvent.UserEvent
                |> inMsg msg
            ]
            if nbRowsUpdated = 1 
            then return Ok ()
            else return Error (SaveUserError.NotFound)
        | Error errors ->
            return Error (SaveUserError.Validation errors)
    else
        return Error (SaveUserError.AuthorizationError)
}

let deleteUser (storage: IStorageEngine) (msg: Message<Guid>) = async {
    if msg.CurrentUser.IsSysAdmin () then
        let! nbRowsDeleted = storage.PersistTransactional [
            msg.Payload
            |> CUDEvent.Deleted
            |> UserEvent.UserEvent
            |> StorageEvent.UserEvent
            |> inMsg msg
        ]
        if nbRowsDeleted = 1
        then return Ok ()
        else return Error (DeleteUserError.NotFound)
    else
        return Error (DeleteUserError.AuthorizationError)
}

module TwoFac =
    let updateTwoFactorAuthentication (storage: IStorageEngine) (twoFacPassword: string) (twoFacPepper: string) (msg: Message<UpdateTwoFacAuthentication>) =
        let encrypted = EncryptedUpdateTwoFacAuthentication.Encrypt twoFacPassword twoFacPepper msg.Payload
        //TODO: send email...
        storage.PersistTransactional [
            encrypted
            |> UserEvent.TwoFactorAuthenticationWasUpdated
            |> StorageEvent.UserEvent
            |> inMsg msg
        ]
        |> Async.Ignore

    let addFailedTwoFacAttempt (storage: IStorageEngine) (msg: Message<FailedTwoFacAttempt>) =
        storage.PersistTransactional [
            msg.Payload
            |> UserEvent.FailedTwoFacAttemptWasAdded
            |> StorageEvent.UserEvent
            |> inMsg msg
        ]
        |> Async.Ignore

    let generateNewRecoveryCodes (storage: IStorageEngine) (twoFacPepper: string) (msg: Message<Guid>) = async {
        let codes =
            [
                Encryption.generateRandomString (6)
                Encryption.generateRandomString (6)
                Encryption.generateRandomString (6)
                Encryption.generateRandomString (6)
                Encryption.generateRandomString (6)
                Encryption.generateRandomString (6)
            ]
        let hashedCodes = codes |> List.map (EncryptedUpdateTwoFacAuthentication.EncryptRecoveryCode twoFacPepper)
        //TODO: send email...
        let! _ = storage.PersistTransactional [
            (msg.Payload, hashedCodes)
            |> UserEvent.RecoveryCodesWereUpdated
            |> StorageEvent.UserEvent
            |> inMsg msg
        ]
        return codes
    }

    let removeUsedRecoveryCode (storage: IStorageEngine) (twoFacPepper: string) (msg: Message<Guid * string>) = async {
        let userId, code = msg.Payload
        let hashedCode = EncryptedUpdateTwoFacAuthentication.EncryptRecoveryCode twoFacPepper code
        let! _ = storage.PersistTransactional [
            (userId, hashedCode)
            |> UserEvent.RecoveryCodeWasUsed
            |> StorageEvent.UserEvent
            |> inMsg msg
        ]
        return ()
    }

module JwtTokens =
    open System.IdentityModel.Tokens.Jwt
    open System.Security.Claims

    let [<Literal>] issuer = "urn.syndicusassistent.authentication.be"
    let private jwtSecurityTokenHandler = new JwtSecurityTokenHandler();

    let private tokenValidationParameters signingKey =
        new TokenValidationParameters (
            ValidIssuer = issuer,
            ValidateIssuer = true,
            ValidateAudience = false,
            ValidateLifetime = true,
            IssuerSigningKey = signingKey,
            ValidateIssuerSigningKey = true,
            RequireSignedTokens = true
        );

    let validateToken (signingKey: SecurityKey) (token: string) =
        if String.IsNullOrEmpty(token)
        then
            None
        else
            try
                let identity, _securityToken = jwtSecurityTokenHandler.ValidateToken(token, tokenValidationParameters signingKey)
                Some identity
            with e ->
                Log.Logger.Error(e, "Something went wrong while validing a security token.")
                None

    let generateToken (signingKey: SecurityKey) (claims: Claim list, expiresAfter: TimeSpan) =
        let securityTokenDescriptor =
            new SecurityTokenDescriptor (
                Subject = new ClaimsIdentity(claims),
                Expires = Nullable(DateTime.UtcNow.Add(expiresAfter)),
                SigningCredentials = new SigningCredentials(signingKey, SecurityAlgorithms.HmacSha256Signature),
                Issuer = issuer
            )

        let securityToken = jwtSecurityTokenHandler.CreateToken(securityTokenDescriptor)
        jwtSecurityTokenHandler.WriteToken(securityToken)
