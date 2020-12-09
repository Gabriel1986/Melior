module Server.Authentication.HttpHandler

open System
open System.Security.Claims
open System.IdentityModel.Tokens.Jwt
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.Http
open FSharp.Data
open FSharp.Control.Tasks.V2
open Giraffe
open Giraffe.Antiforgery
open Giraffe.GiraffeViewEngine
open Google.Authenticator
open Serilog
open MimeKit
open MimeKit.Text

open Shared.Read
open Server
open Server.AppSettings
open Server.Blueprint.Behavior.Authentication
open Server.Blueprint.Data.Authentication
open Server.Library
open Server.LibraryExtensions
open Pages
open Emails

let [<Literal>] googleRecaptchaResponseExample =
    """
        {
          "success": true,
          "challenge_ts": "2019-04-20T11:13:14",
          "hostname": "Some host name",
          "error-codes": [ "Some error code" ]
        }
    """

type GoogleRecaptchaResponse = JsonProvider<googleRecaptchaResponseExample>

let cookieAuthenticationProperties: AuthenticationProperties = 
    new AuthenticationProperties (
        AllowRefresh = Nullable(false),
        IsPersistent = false
    )
   
[<CLIMutable>]
type TokenRequestParams = {
    Token: string
}

[<CLIMutable>]
type LoginForm = {
    UserName: string
    Password: string
}

[<CLIMutable>]
type ChangePasswordForm = {
    NewPassword1: string
    NewPassword2: string
}

[<CLIMutable>]
type ForgotPasswordForm = {
    UserName: string
}

[<CLIMutable>]
type RecommendTwoFactorAuthenticationForm = {
    SubmitButton: string
}

[<CLIMutable>]
type TwoFacLoginForm = {
    VerificationCode: string
    SubmitButton: string
}

[<CLIMutable>]
type TwoFacRecoveryForm = {
    RecoveryCode: string
}

let private usernamePasswordTokenValidity = TimeSpan.FromMinutes(15.0)
let private resetPasswordTokenValidity = TimeSpan.FromHours(1.0)
let private sharedSecretCache = FSharp.Data.Runtime.Caching.createInMemoryCache (usernamePasswordTokenValidity)

let createAuthenticationHandler (settings: AppSettings) (system: IAuthenticationSystem): HttpHandler =
    let loginAuthSchema = CookieAuthenticationDefaults.AuthenticationScheme
    let toClaimsPrincipal = User.ToClaimsPrincipal loginAuthSchema

    let createMessage (ctx: HttpContext) (payload: 'T): Message<'T> = {
        CreatedAt = DateTimeOffset.UtcNow
        Context = Some ctx
        Payload = payload
    }

    let googleRecaptchSubmitButton btnText =
        div [ _style "display:inline-block;" ] [
            script [ _type "application/javascript" ] [
                rawText
                    """
                        var onSubmit = function(token) {
                            document.forms[0].submit();
                        };
                        window.onSubmit = onSubmit;
                    """
            ]
            button [ 
                _class "g-recaptcha btn btn-primary"
                attr "data-sitekey" settings.Google.RecaptchaClientKey
                attr "data-callback" "onSubmit" 
                attr "data-action" "submit"
            ] [ str btnText ]
        ]

    let requiresRecaptchaToken (invalidTokenHandler: HttpHandler) = (fun nxt (ctx: HttpContext) -> 
        task {
            let token = defaultArg (ctx.GetFormValue "g-recaptcha-response") "This should not occur..."
            let body: HttpRequestBody = seq { "secret", settings.Google.RecaptchaServerKey; "response", token } |> FormValues
            let! stringResponse = Http.AsyncRequestString("https://www.google.com/recaptcha/api/siteverify?", httpMethod = "POST", body = body)
            let parsedResponse = GoogleRecaptchaResponse.Parse(stringResponse)
            printfn "%A" parsedResponse
            if not parsedResponse.Success then Log.Logger.Error(sprintf "A recaptcha was invalid, details: %s" stringResponse)
            return!
                if parsedResponse.Success 
                then nxt ctx
                else invalidTokenHandler nxt ctx
        }
    )

    choose [
        //TODO: https://docs.microsoft.com/en-us/aspnet/core/security/authentication/cookie?view=aspnetcore-3.1
        routeCi "/authentication/changePassword" >=> choose [
            GET  >=> (fun nxt ctx ->
                let requestParams = ctx.BindQueryString<TokenRequestParams>()
                match system.ValidateChangePasswordToken requestParams.Token with 
                | Some _ -> csrfHtmlView (changePasswordPage googleRecaptchSubmitButton []) nxt ctx
                | None   -> htmlView passwordTokenExpiredPage nxt ctx)
            POST 
                >=> requiresCsrfToken (text "CSRF token validation failed...") 
                >=> requiresRecaptchaToken (csrfHtmlView (changePasswordPage googleRecaptchSubmitButton [ "Google recaptcha is niet geldig" ]))
                >=> (fun nxt ctx -> task {
                    let requestParams = ctx.BindQueryString<TokenRequestParams>()
                    match system.ValidateChangePasswordToken requestParams.Token with
                    | None -> 
                        return! htmlView passwordTokenExpiredPage nxt ctx
                    | Some subject ->
                        let! submitParams = ctx.BindFormAsync<ChangePasswordForm>()
                        let parsePassword password = (if String.IsNullOrWhiteSpace (password) then "" else password).Trim()

                        let password = parsePassword submitParams.NewPassword1
                        let password2 = parsePassword submitParams.NewPassword2

                        let validationErrors = [
                            if password.Length < 8 then
                                yield "Een wachtwoord moet minimaal 8 cijfers en tekens bevatten"
                            if password.Length > 64 then
                                yield "Een wachtwoord mag maximaal 64 cijfers en tekens bevatten"
                            if CommonPasswords.isCommonPassword password then
                                yield "Het door u opgegeven wachtwoord is een veel voorkomend wachtwoord, gelieve een ander wachtwoord in te geven..."
                            if not (password |> String.exists(fun c -> Char.IsDigit c)) then
                                yield "Het wachtwoord moet minstens 1 cijfer bevatten"
                            if not (password |> String.exists(fun c -> Char.IsLetter c)) then
                                yield "Het wachtwoord moet minstens 1 letter bevatten"
                            if password <> password2 then
                                yield "De wachtwoorden zijn niet gelijk"
                        ]
                        match validationErrors with
                        | [] -> 
                            let! user =
                                (subject.FindFirstValue(JwtRegisteredClaimNames.Sub))
                                |> createMessage ctx
                                |> system.FindUserByEmailAddress
                            match user with
                            | None ->
                                return! csrfHtmlView (changePasswordPage googleRecaptchSubmitButton [ "Er werd geen gebruiker gevonden voor het opgegeven e-mail adres" ]) nxt ctx
                            | Some user ->
                                let! result = 
                                    (user.UserId, password)
                                    |> createMessage ctx
                                    |> system.UpdatePassword
                                match result with
                                | Ok () -> return! redirectTo false "/authentication/login" nxt ctx
                                | Error e -> return! htmlView somethingWentWrongPage nxt ctx
                        | errors -> 
                            return! csrfHtmlView (changePasswordPage googleRecaptchSubmitButton errors) nxt ctx                    
            })
        ]
        routeCi "/authentication/forgotPassword" >=> choose [
            GET >=> csrfHtmlView (forgotPasswordPage googleRecaptchSubmitButton [])
            POST 
                >=> requiresCsrfToken (text "CSRF token validation failed...") 
                >=> requiresRecaptchaToken (csrfHtmlView (forgotPasswordPage googleRecaptchSubmitButton [ "Google recaptcha is niet geldig" ]))
                >=> (fun nxt ctx  -> task {
                    let! requestParams = ctx.BindFormAsync<ForgotPasswordForm>()
                    let emailAddress = requestParams.UserName
                    let! user =
                        emailAddress
                        |> createMessage ctx
                        |> system.FindUserByEmailAddress
                    match user with
                    | Some user ->
                        let resetPasswordToken = system.GenerateChangePasswordToken ([ new Claim(JwtRegisteredClaimNames.Sub, emailAddress) ], resetPasswordTokenValidity)
                        let sendSupportMail = Server.MailProvider.sendSupportEmail settings.Mail
                        let receiver = new MailboxAddress(user.DisplayName, emailAddress)
                        let body = 
                            new TextPart(
                                format = TextFormat.Html, 
                                Text = renderHtmlNodes [ resetPasswordEmail (user, sprintf "%s/authentication/changePassword?Token=%s" settings.BaseUrl resetPasswordToken) ]
                            )
                        match! sendSupportMail [ receiver ] ("Wachtwoord opnieuw instellen", body) with
                        | Ok () -> return! htmlView resetPasswordMailSent nxt ctx
                        | Error e -> return! htmlView resetPasswordMailSendingFailed nxt ctx
                    | None ->
                        return! htmlView resetPasswordMailSendingFailed nxt ctx
                })
        ]
        routeCi "/authentication/login" >=> choose [ 
            GET  >=> csrfHtmlView (loginPage googleRecaptchSubmitButton [])
            POST 
                >=> requiresCsrfToken (text "CSRF token validation failed...")
                >=> requiresRecaptchaToken (csrfHtmlView (loginPage googleRecaptchSubmitButton [ "Google recaptcha is niet geldig" ]))
                >=> (fun nxt ctx -> task {
                    let! loginParams = ctx.BindFormAsync<LoginForm>()
                    let! authenticatedUserResult = 
                        (loginParams.UserName, loginParams.Password)
                        |> createMessage ctx
                        |> system.AuthenticateUser 

                    match authenticatedUserResult with
                    | Ok authenticatedUser ->
                        let claims = [ 
                            new Claim (JwtRegisteredClaimNames.Sub, string authenticatedUser.UserId) 
                            new Claim (JwtRegisteredClaimNames.Email, authenticatedUser.EmailAddress)
                            new Claim ("use2fac", if authenticatedUser.UseTwoFac then "1" else "0")
                        ]
                        let validatedUsernamePasswordToken = system.GenerateUsernamePasswordToken (claims, usernamePasswordTokenValidity)
                        return! redirectTo false (sprintf "/authentication/2fac?Token=%s" validatedUsernamePasswordToken) nxt ctx
                    | Error AuthenticationError.UserNotFound
                    | Error AuthenticationError.PasswordNotValid ->
                        return! csrfHtmlView (loginPage googleRecaptchSubmitButton [ "Gebruiker niet gevonden of wachtwoord niet geldig" ]) nxt ctx
                })
        ]
        routeCi "/authentication/recommend2fac" >=> choose [
            GET >=> (fun nxt ctx ->
                let requestParams = ctx.BindQueryString<TokenRequestParams>()
                match system.ValidateUsernamePasswordToken requestParams.Token with 
                | Some _identity ->
                    csrfHtmlView recommendTwoFacPage nxt ctx
                | None ->
                    htmlView usernamePasswordTokenExpiredPage nxt ctx
            )
            POST 
                >=> requiresCsrfToken (text "CSRF token validation failed...")
                >=> (fun nxt ctx -> task {
                let! postParams = ctx.BindFormAsync<RecommendTwoFactorAuthenticationForm>()
                let requestParams = ctx.BindQueryString<TokenRequestParams>()
                match system.ValidateUsernamePasswordToken requestParams.Token with
                | Some identity ->
                    match postParams.SubmitButton with
                    | "UseTwoFac" ->
                        return! redirectTo false (sprintf "/authentication/enable2fac?Token=%s" requestParams.Token) nxt ctx
                    | _ ->
                        let! user =
                            identity.FindFirstValue (JwtRegisteredClaimNames.Sub) 
                            |> Guid.Parse
                            |> createMessage ctx
                            |> system.GetUser
                        match user with
                        | Some currentUser ->
                            let claimsPrincipal = toClaimsPrincipal currentUser
                            do! ctx.SignInAsync(loginAuthSchema, claimsPrincipal)
                            return! redirectTo false "/" nxt ctx
                        | None ->
                            //This should never occur really... But meh :)
                            return! htmlView usernamePasswordTokenExpiredPage nxt ctx
                | None ->
                    return! htmlView usernamePasswordTokenExpiredPage nxt ctx
            })
        ]
        routeCi "/authentication/enable2fac" >=> choose [
            GET >=> (fun nxt ctx -> task {
                let requestParams = ctx.BindQueryString<TokenRequestParams>()
                match system.ValidateUsernamePasswordToken requestParams.Token with 
                | Some identity ->
                    let userEmailAddress = identity.FindFirstValue(JwtRegisteredClaimNames.Email)
                    let secret = Encryption.generateRandomString(16)
                    let recoveryCodes = [
                        Encryption.generateRandomString(8)
                        Encryption.generateRandomString(8)
                        Encryption.generateRandomString(8)
                        Encryption.generateRandomString(8)
                    ]

                    //Making sure the secret doesn't go over the wire twice.
                    do sharedSecretCache.Set(requestParams.Token, (secret, recoveryCodes))

                    let tfa = new TwoFactorAuthenticator()
                    let setupInfo = tfa.GenerateSetupCode("SyndicusAssistent", userEmailAddress, secret, false, 4)
                    let qrCodeImageUrl = setupInfo.QrCodeSetupImageUrl; //  assigning the Qr code information + URL to a base64 encoded string
                    let manualEntrySetupCode = setupInfo.ManualEntryKey; // show the Manual Entry Key for the users that don't have app or phone

                    return! csrfHtmlView (initializeTwoFacPage qrCodeImageUrl manualEntrySetupCode []) nxt ctx
                | None ->
                    return! htmlView usernamePasswordTokenExpiredPage nxt ctx
            })
            POST
                >=> requiresCsrfToken (text "CSRF token validation failed...")
                >=> (fun nxt ctx -> task {
                let! postParams = ctx.BindFormAsync<TwoFacLoginForm>()
                let requestParams = ctx.BindQueryString<TokenRequestParams>()
                match system.ValidateUsernamePasswordToken requestParams.Token with 
                | Some identity ->
                    match postParams.SubmitButton with
                    | "Cancel" ->
                        let! user =
                            identity.FindFirstValue (JwtRegisteredClaimNames.Sub) 
                            |> Guid.Parse
                            |> createMessage ctx
                            |> system.GetUser
                        match user with
                        | Some currentUser ->
                            let claimsPrincipal = toClaimsPrincipal currentUser
                            do! ctx.SignInAsync(loginAuthSchema, claimsPrincipal)
                            return! redirectTo false "/" nxt ctx
                        | None ->
                            //This should never occur really... But meh :)
                            return! htmlView usernamePasswordTokenExpiredPage nxt ctx
                    | _ ->
                        let tfa = new TwoFactorAuthenticator();
                        let (secret, recoveryCodes): string * string list = defaultArg (sharedSecretCache.TryRetrieve(requestParams.Token)) ("", [])
                        match tfa.ValidateTwoFactorPIN(secret, postParams.VerificationCode, TimeSpan.FromMinutes(3.0)) with
                        | true ->
                            let! user =
                                identity.FindFirstValue (JwtRegisteredClaimNames.Sub) 
                                |> Guid.Parse
                                |> createMessage ctx
                                |> system.GetUser
                            match user with
                            | Some currentUser ->
                                let updateTwoFac = {
                                    UserId = currentUser.UserId
                                    UseTwoFac = true
                                    TwoFacSecret = secret
                                    RecoveryCodes = recoveryCodes
                                }
                                do! system.UpdateTwoFacAuthentication updateTwoFac
                                //TODO: send email for recoveryCodes
                                let claimsPrincipal = toClaimsPrincipal currentUser
                                do! ctx.SignInAsync(loginAuthSchema, claimsPrincipal)
                                return! htmlView twoFacSucceeded nxt ctx
                            | None ->
                                //This should never occur really... But meh :)
                                return! htmlView usernamePasswordTokenExpiredPage nxt ctx
                        | false ->
                            let userEmailAddress = identity.FindFirstValue(JwtRegisteredClaimNames.Email)
                            do sharedSecretCache.Set(requestParams.Token, (secret, recoveryCodes))

                            let tfa = new TwoFactorAuthenticator()
                            let setupInfo = tfa.GenerateSetupCode("SyndicusAssistent", userEmailAddress, secret, false, 4)
                            let qrCodeImageUrl = setupInfo.QrCodeSetupImageUrl; //  assigning the Qr code information + URL to a base64 encoded string
                            let manualEntrySetupCode = setupInfo.ManualEntryKey; // show the Manual Entry Key for the users that don't have app or phone

                            return! csrfHtmlView (initializeTwoFacPage qrCodeImageUrl manualEntrySetupCode [ "De ingegeven code was niet geldig. Gelieve opnieuw te proberen." ]) nxt ctx
                | None ->
                    return! htmlView usernamePasswordTokenExpiredPage nxt ctx                
            })
        ]
        routeCi "/authentication/2fac" >=> choose [
            GET  >=> (fun nxt ctx -> task {
                let requestParams = ctx.BindQueryString<TokenRequestParams>()
                match system.ValidateUsernamePasswordToken requestParams.Token with 
                | Some identity ->
                    let! nbAttempts = system.GetNbFailedTwoFacAttempts (identity.FindFirstValue(JwtRegisteredClaimNames.Sub) |> Guid.Parse, DateTimeOffset.UtcNow.Subtract(TimeSpan.FromMinutes(15.0)))
                    if nbAttempts > 5 then
                        return! htmlView tooManyFailedTwoFacAttempts nxt ctx
                    else
                        match identity.FindFirstValue("use2fac") with
                        | x when x = "1" ->
                            return! csrfHtmlView (twoFacPage [] requestParams.Token) nxt ctx
                        | _ ->
                            return! redirectTo false (sprintf "/authentication/recommend2fac?Token=%s" requestParams.Token) nxt ctx
                | None   -> 
                    return! htmlView usernamePasswordTokenExpiredPage nxt ctx
            })
            POST 
                >=> requiresCsrfToken (text "CSRF token validation failed...")
                >=> (fun nxt ctx -> task {
                    let! postParams = ctx.BindFormAsync<TwoFacLoginForm>()
                    let requestParams = ctx.BindQueryString<TokenRequestParams>()
                    match system.ValidateUsernamePasswordToken requestParams.Token with
                    | Some identity ->
                        let! user =
                            identity.FindFirstValue (JwtRegisteredClaimNames.Sub) 
                            |> Guid.Parse
                            |> createMessage ctx
                            |> system.GetUser
                        match user with
                        | Some currentUser ->
                            match! system.ValidateTwoFactorPIN(currentUser.UserId, postParams.VerificationCode) with
                            | true ->
                                let claimsPrincipal = toClaimsPrincipal currentUser
                                do! ctx.SignInAsync(loginAuthSchema, claimsPrincipal)
                                return! redirectTo false "/" nxt ctx
                            | false ->
                                do! system.AddFailedTwoFacAttempt { UserId = currentUser.UserId; Timestamp = DateTimeOffset.UtcNow }
                                let! nbAttempts = system.GetNbFailedTwoFacAttempts (currentUser.UserId, DateTimeOffset.UtcNow.Subtract(TimeSpan.FromMinutes(15.0)))
                                if nbAttempts > 5 then
                                    return! htmlView tooManyFailedTwoFacAttempts nxt ctx
                                else
                                    return! csrfHtmlView (twoFacPage [ "De ingegeven code was niet geldig. Gelieve opnieuw te proberen." ] requestParams.Token) nxt ctx
                        | None ->
                            //This should never occur really... But meh :)
                            return! htmlView usernamePasswordTokenExpiredPage nxt ctx
                    | None ->
                        return! htmlView usernamePasswordTokenExpiredPage nxt ctx                        
                })
        ]
        routeCi "/authentication/enter2facRecoveryCode" >=> choose [
            GET >=> (fun nxt ctx -> task {
                let requestParams = ctx.BindQueryString<TokenRequestParams>()
                match system.ValidateUsernamePasswordToken requestParams.Token with 
                | Some identity ->
                    let! nbAttempts = system.GetNbFailedTwoFacAttempts (identity.FindFirstValue(JwtRegisteredClaimNames.Sub) |> Guid.Parse, DateTimeOffset.UtcNow.Subtract(TimeSpan.FromMinutes(15.0)))
                    if nbAttempts > 5 then
                        return! htmlView tooManyFailedTwoFacAttempts nxt ctx
                    else
                        return! csrfHtmlView (twoFacRecoveryCodePage [] requestParams.Token) nxt ctx
                | None ->
                    return! htmlView usernamePasswordTokenExpiredPage nxt ctx
            })
            POST 
                >=> requiresCsrfToken (text "CSRF token validation failed...")
                >=> (fun nxt ctx -> task {
                    let! postParams = ctx.BindFormAsync<TwoFacRecoveryForm>()
                    let requestParams = ctx.BindQueryString<TokenRequestParams>()
                    match system.ValidateUsernamePasswordToken requestParams.Token with
                    | Some identity ->
                        let! user = 
                            identity.FindFirstValue (JwtRegisteredClaimNames.Sub) 
                            |> Guid.Parse
                            |> createMessage ctx
                            |> system.GetUser
                        match user with
                        | Some currentUser ->
                            match! system.ValidateRecoveryCode(currentUser.UserId, postParams.RecoveryCode) with
                            | true ->
                                do! system.RemoveUsedRecoveryCode(currentUser.UserId, postParams.RecoveryCode)
                                let claimsPrincipal = toClaimsPrincipal currentUser
                                do! ctx.SignInAsync(loginAuthSchema, claimsPrincipal)
                                return! redirectTo false "/" nxt ctx
                            | false ->
                                do! system.AddFailedTwoFacAttempt { UserId = currentUser.UserId; Timestamp = DateTimeOffset.UtcNow }
                                let! nbAttempts = system.GetNbFailedTwoFacAttempts (currentUser.UserId, DateTimeOffset.UtcNow.Subtract(TimeSpan.FromMinutes(15.0)))
                                if nbAttempts > 5 then
                                    return! htmlView tooManyFailedTwoFacAttempts nxt ctx
                                else
                                    return! csrfHtmlView (twoFacPage [ "De ingegeven code was niet geldig. Gelieve opnieuw te proberen." ] requestParams.Token) nxt ctx
                        | None ->
                            //This should never occur really... But meh :)
                            return! htmlView usernamePasswordTokenExpiredPage nxt ctx
                    | None ->
                        return! htmlView usernamePasswordTokenExpiredPage nxt ctx                        
                })
        ]
        routeCi "/authentication/accessDenied" >=> 
            GET >=> htmlView accessDeniedPage
        routeCi "/authentication/logout" >=>
            GET >=> signOut CookieAuthenticationDefaults.AuthenticationScheme >=> redirectTo false "/"
    ]