module Server.Authentication.AuthenticationSystem

open System
open Giraffe
open Server.Blueprint.Behavior.Authentication
open Server.AppSettings
open Microsoft.IdentityModel.Tokens

let build (settings: AppSettings) =
    let conn = settings.Database.ConnectionString
    let changePasswordSigningKey = new SymmetricSecurityKey(Convert.FromBase64String(settings.Authentication.ChangePasswordSigningKey))
    let usernamePasswordSigningKey = new SymmetricSecurityKey(Convert.FromBase64String(settings.Authentication.UsernamePasswordSigningKey))
    let twoFacPassword = settings.Authentication.TwoFacPassword
    let passwordPepper = settings.Authentication.PasswordPepper

    {
        new IAuthenticationSystem with
            //Commands
            member _.DisableAccount userId = 
                Workflows.disableAccount conn userId
            
            member _.EnableAccount userId = 
                Workflows.enableAccount conn userId

            member _.GenerateChangePasswordToken (claims, expiresAfter) = 
                Workflows.JwtTokens.generateToken changePasswordSigningKey (claims, expiresAfter)
            
            member _.ValidateChangePasswordToken token = 
                Workflows.JwtTokens.validateToken changePasswordSigningKey token
            
            member _.GenerateUsernamePasswordToken (claims, expiresAfter) = 
                Workflows.JwtTokens.generateToken usernamePasswordSigningKey (claims, expiresAfter)
            
            member _.ValidateUsernamePasswordToken token = 
                Workflows.JwtTokens.validateToken usernamePasswordSigningKey token
            
            member _.ValidateTwoFactorPIN (userId, verificationCode) = 
                Workflows.TwoFac.validateTwoFactorPIN conn twoFacPassword (userId, verificationCode)
            
            member _.UpdateTwoFacAuthentication update = 
                Workflows.TwoFac.updateTwoFactorAuthentication twoFacPassword update
            
            member _.RemoveUsedRecoveryCode (email, code) = 
                Workflows.TwoFac.removeUsedRecoveryCode (email, code)
            
            member _.GenerateNewRecoveryCodes email = 
                Workflows.TwoFac.generateNewRecoveryCodes email
            
            member _.AddFailedTwoFacAttempt failedAttempt = 
                Workflows.TwoFac.addFailedTwoFacAttempt failedAttempt

            //Queries
            member _.GetTwoFacPassword userId =
                Query.getTwoFacPassword conn twoFacPassword userId
            
            member _.AuthenticateUser (emailAddress, password) =
                Query.authenticateUser conn passwordPepper (emailAddress, password)
            
            member _.FindUserByEmailAddress (emailAddress) =
                Query.findUserByEmailAddress conn emailAddress
            
            member _.GetUser userId =
                Query.getUser conn userId
            
            member _.GetNbFailedTwoFacAttempts (userId, after) =
                Query.getNbFailedTwoFacAttempts conn (userId, after)

            member me.HttpHandler =
                HttpHandler.createAuthenticationHandler me settings
    }