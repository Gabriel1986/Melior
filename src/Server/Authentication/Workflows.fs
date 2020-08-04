module Server.Authentication.Workflows
    open System
    open Serilog
    open Server.Blueprint.Data.Authentication
    open Microsoft.IdentityModel.Tokens
    open Shared.Library
    open Google.Authenticator

    let disableAccount conn (userId: Guid) =
        Async.lift ()

    let enableAccount conn (userId: Guid) =
        Async.lift ()

    module TwoFac =
        let validateTwoFactorPIN (conn) (twoFacEncryptionPassword) (userId, verificationCode) = async {
            let! twoFacSharedPassword = Query.getTwoFacPassword conn twoFacEncryptionPassword userId
            match twoFacSharedPassword with
            | Some twoFacSharedPassword ->
                let tfa = new TwoFactorAuthenticator();
                return tfa.ValidateTwoFactorPIN(twoFacSharedPassword, verificationCode, TimeSpan.FromMinutes(2.0))
            | None ->
                return false
        }

        let updateTwoFactorAuthentication (password: string) (updateTwoFactorAuthentication: UpdateTwoFacAuthentication) =
            Async.lift ()

        let addFailedTwoFacAttempt (attempt: FailedTwoFacAttempt) =
            Async.lift ()

        let generateNewRecoveryCodes (email: string) =
            Async.lift []

        let removeUsedRecoveryCode (email: string, code: string) =
            Async.lift ()

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
                    SigningCredentials = new SigningCredentials(signingKey, SecurityAlgorithms.RsaSha256Signature),
                    Issuer = issuer
                )

            let securityToken = jwtSecurityTokenHandler.CreateToken(securityTokenDescriptor)
            jwtSecurityTokenHandler.WriteToken(securityToken)
