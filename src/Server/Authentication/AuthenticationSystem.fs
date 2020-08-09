module Server.Authentication.AuthenticationSystem

open System
open Microsoft.IdentityModel.Tokens
open Microsoft.Extensions.Configuration
open Server.Blueprint.Behavior.Authentication
open Server.AppSettings
open Server.LibraryExtensions
open Server.Blueprint.Behavior.ProfessionalSyndics

let build (config: IConfiguration) =
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.ConnectionString
    let changePasswordSigningKey = new SymmetricSecurityKey(Convert.FromBase64String(settings.Authentication.ChangePasswordSigningKey))
    let usernamePasswordSigningKey = new SymmetricSecurityKey(Convert.FromBase64String(settings.Authentication.UsernamePasswordSigningKey))
    let twoFacPassword = settings.Authentication.TwoFacPassword
    let twoFacPepper = settings.Authentication.TwoFacPepper
    let passwordPepper = settings.Authentication.PasswordPepper

    let storage = Storage.makeStorage settings

    {
        new IAuthenticationSystem with
            //Commands (internal)
            member _.GenerateChangePasswordToken (claims, expiresAfter) = 
                Workflows.JwtTokens.generateToken changePasswordSigningKey (claims, expiresAfter)
            
            member _.ValidateChangePasswordToken token = 
                Workflows.JwtTokens.validateToken changePasswordSigningKey token
            
            member _.GenerateUsernamePasswordToken (claims, expiresAfter) = 
                Workflows.JwtTokens.generateToken usernamePasswordSigningKey (claims, expiresAfter)
            
            member _.ValidateUsernamePasswordToken token = 
                Workflows.JwtTokens.validateToken usernamePasswordSigningKey token
            
            member _.UpdateTwoFacAuthentication update = 
                Workflows.TwoFac.updateTwoFactorAuthentication storage twoFacPassword twoFacPepper update
            
            member _.RemoveUsedRecoveryCode (userId, code) = 
                Workflows.TwoFac.removeUsedRecoveryCode storage twoFacPepper (userId, code)
            
            member _.GenerateNewRecoveryCodes (userId, email) = 
                Workflows.TwoFac.generateNewRecoveryCodes storage twoFacPepper (userId, email)
            
            member _.AddFailedTwoFacAttempt failedAttempt = 
                Workflows.TwoFac.addFailedTwoFacAttempt storage failedAttempt

            member _.AddUser user =
                Workflows.addUser storage passwordPepper user

            //Queries (interal)
            member _.AuthenticateUser msg =
                Query.authenticateUser conn msg.ProfessionalSyndicCache passwordPepper msg.Payload
            
            member _.ValidateRecoveryCode (userId, recoveryCode) =
                Query.validateRecoveryCode conn twoFacPepper (userId, recoveryCode)

            member _.ValidateTwoFactorPIN (userId, verificationCode) =
                Query.validateTwoFactorPIN conn twoFacPassword (userId, verificationCode)

            member _.FindUserByEmailAddress msg =
                Query.findUserByEmailAddress conn msg.ProfessionalSyndicCache msg.Payload

            member _.UserWithEmailAddressExists emailAddress =
                Query.userWithEmailAddressExists conn emailAddress
            
            member _.GetUser msg =
                Query.getUser conn msg.ProfessionalSyndicCache msg.Payload
            
            member _.GetNbFailedTwoFacAttempts (userId, after) =
                Query.getNbFailedTwoFacAttempts conn (userId, after)

            member me.HttpHandler =
                HttpHandler.createAuthenticationHandler settings me
    }