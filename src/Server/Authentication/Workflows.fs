module Server.Authentication.Workflows
    open System
    open Serilog
    open Server.Blueprint.Data.Authentication
    open Microsoft.IdentityModel.Tokens
    open Shared.Library
    open Storage
    open Server.Library

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

    let updatePassword (storage: IAuthenticationStorage) (passwordPepper: string) (msg: Message<Guid * string>) = async {
        let passwordHash = Encryption.hashPassword passwordPepper (snd msg.Payload)
        let! nbUsersUpdated = storage.UpdatePassword (fst msg.Payload, passwordHash)
        return
            if nbUsersUpdated <> 1
            then Error (exn "Something went terribly wrong while updating the user's password O_o")
            else Ok ()
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
