module Server.Authentication.Workflows
    open System
    open Serilog
    open Server.Blueprint.Data.Authentication
    open Microsoft.IdentityModel.Tokens
    open Shared.Library
    open Storage
    open Server.Library
    open Server.LibraryExtensions
    open Shared.Remoting
    open Shared.Write
    open Shared.Read

    //Should be used internally only!
    let addUser (storage: IAuthenticationStorage) (passwordPepper: string) (user: UserInput) = async {
        let validated = ValidatedUserInput.Validate passwordPepper user
        match validated with
        | Ok validated -> 
            do! storage.AddUser validated
            return Ok ()
        | Error errors -> 
            return Error (CreateUserError.ValidationError errors)
    }

    let createUser (storage: IAuthenticationStorage) (msg: Message<User>) = async {
        if msg.CurrentUser.IsSysAdmin () then
            let validated = ValidatedUser.Validate msg.Payload
            //TODO: send email...
            match validated with
            | Ok validated ->
                do! storage.CreateUser validated
                return Ok ()
            | Error errors ->
                return Error (SaveUserError.Validation errors)
        else
            return Error (SaveUserError.AuthorizationError)
    }

    let updateUser (storage: IAuthenticationStorage) (msg: Message<User>) = async {
        if msg.CurrentUser.IsSysAdmin () then
            let validated = ValidatedUser.Validate msg.Payload
            //TODO: send email...
            match validated with
            | Ok validated ->
                let! nbRowsUpdated = storage.UpdateUser validated
                if nbRowsUpdated = 1 
                then return Ok ()
                else return Error (SaveUserError.NotFound)
            | Error errors ->
                return Error (SaveUserError.Validation errors)
        else
            return Error (SaveUserError.AuthorizationError)
    }

    let deleteUser (storage: IAuthenticationStorage) (msg: Message<Guid>) = async {
        if msg.CurrentUser.IsSysAdmin () then
            let! nbRowsDeleted = storage.DeleteUser msg.Payload
            if nbRowsDeleted = 1
            then return Ok ()
            else return Error (DeleteUserError.NotFound)
        else
            return Error (DeleteUserError.AuthorizationError)
    }

    module TwoFac =
        let updateTwoFactorAuthentication (storage: IAuthenticationStorage) (twoFacPassword: string) (twoFacPepper: string) (updateTwoFactorAuthentication: UpdateTwoFacAuthentication) = async {
            let encrypted = EncryptedUpdateTwoFacAuthentication.Encrypt twoFacPassword twoFacPepper updateTwoFactorAuthentication
            //TODO: send email...
            let! _ = storage.UpdateTwoFacAuthentication encrypted
            return ()
        }

        let addFailedTwoFacAttempt (storage: IAuthenticationStorage) (attempt: FailedTwoFacAttempt) =
            storage.AddFailedTwoFacAttempt attempt

        let generateNewRecoveryCodes (storage: IAuthenticationStorage) (twoFacPepper: string) (userId: Guid, email: string) = async {
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
            let! _ = storage.UpdateRecoveryCodes (userId, hashedCodes)
            return codes
        }

        let removeUsedRecoveryCode (storage: IAuthenticationStorage) (twoFacPepper: string) (userId: Guid, code: string) = async {
            let hashedCode = EncryptedUpdateTwoFacAuthentication.EncryptRecoveryCode twoFacPepper code
            let! _ = storage.RemoveUsedRecoveryCode (userId, hashedCode)
            return ()
        }

    module JwtTokens =
        open System.IdentityModel.Tokens.Jwt
        open System.Security.Claims

        let [<Literal>] issuer = "urn.melior.authentication.be"
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
