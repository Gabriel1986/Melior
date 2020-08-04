module Server.Authentication.HttpHandler

open System
open System.Security.Claims
open System.Text.RegularExpressions
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.Extensions.Configuration
open Microsoft.AspNetCore.Http
open FSharp.Data
open FSharp.Control.Tasks.V2
open Giraffe
open Giraffe.Antiforgery
open Giraffe.GiraffeViewEngine

open Shared.Read
open Server.AppSettings
open System.IdentityModel.Tokens.Jwt
open Serilog
open Microsoft.IdentityModel.Tokens
open Pages
open Google.Authenticator
open Server.Blueprint.Behavior.Authentication
open Server.Blueprint.Data.Authentication

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
   
type TokenRequestParams = {
    Token: string
}

type LoginForm = {
    UserName: string
    Password: string
}

type ChangePasswordForm = {
    NewPassword1: string
    NewPassword2: string
}

type ForgotPasswordForm = {
    UserName: string
}

type RecommendTwoFactorAuthenticationForm = {
    SubmitButton: string
}

type TwoFacLoginForm = {
    VerificationCode: string
}

let usernamePasswordTokenValidity = TimeSpan.FromMinutes(15.0)
let resetPasswordTokenValidity = TimeSpan.FromHours(1.0)
let sharedSecretCache = FSharp.Data.Runtime.Caching.createInMemoryCache (usernamePasswordTokenValidity)

let createAuthenticationHandler (system: IAuthenticationSystem) (settings: AppSettings): HttpHandler =
    let loginAuthSchema = CookieAuthenticationDefaults.AuthenticationScheme

    let googleRecaptchSubmitButton btnText =
        div [] [
            script [] [
                str 
                    """
                        function onSubmit(token) {
                            let forms = document.getElementsByTagName("form");                          
                            for (let form of forms) {
                                form.submit();
                            }
                        }
                    """
            ]
            button [ 
                _class "g-recaptcha";
                attr "data-sitekey" settings.Google.RecaptchaClientKey
                attr "data-callback" "onSubmit" 
                attr "data-action" "submit"
            ] [ str btnText ]
        ]
   
    let toClaimsPrincipal (user: User) =
        let claims = [
            yield  new Claim(JwtRegisteredClaimNames.Sub, string user.UserId)
            yield  new Claim(JwtRegisteredClaimNames.Email, user.EmailAddress)
            yield  new Claim(JwtRegisteredClaimNames.GivenName, user.DisplayName)
            yield! user.Roles |> List.map (fun role -> new Claim("role", string role))
        ]
        let identity = ClaimsIdentity(claims, loginAuthSchema)
        ClaimsPrincipal(identity)

    let requiresRecaptchaToken (invalidTokenHandler: HttpHandler) = (fun nxt (ctx: HttpContext) -> 
        task {
            let token = defaultArg (ctx.GetFormValue "g-recaptcha-response") "This should not occur..."
            let body: HttpRequestBody = seq { "secret", settings.Google.RecaptchaServerKey; "response", token } |> FormValues
            let! stringResponse = Http.AsyncRequestString("https://www.google.com/recaptcha/api/siteverify?", httpMethod = "POST", body = body)
            let parsedResponse = GoogleRecaptchaResponse.Parse(stringResponse)
            if not parsedResponse.Success then Log.Logger.Error(sprintf "A recaptcha was invalid, details: %s" stringResponse)
            return!
                if parsedResponse.Success 
                then invalidTokenHandler nxt ctx
                else nxt ctx
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
                            if not (Regex.Match(password, @"/\d+/", RegexOptions.ECMAScript).Success) then
                                yield "Het wachtwoord moet minstens 1 cijfer bevatten"
                            if not (Regex.Match(password, @"/[a-zA-Z]+/", RegexOptions.ECMAScript).Success) then
                                yield "Het wachtwoord moet minstens 1 letter bevatten"
                            if password <> password2 then
                                yield "De wachtwoorden zijn niet gelijk"
                        ]
                        match validationErrors with
                        | [] -> 
                            let! user = system.FindUserByEmailAddress (subject.FindFirstValue(JwtRegisteredClaimNames.Sub))
                            match user with
                            | None ->
                                return! csrfHtmlView (changePasswordPage googleRecaptchSubmitButton [ "Er werd geen gebruiker gevonden voor het opgegeven e-mail adres" ]) nxt ctx
                            | Some user ->
                                return! redirectTo false "/authentication/login" nxt ctx
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
                    let resetPasswordToken = system.GenerateChangePasswordToken ([ new Claim(JwtRegisteredClaimNames.Sub, emailAddress) ], resetPasswordTokenValidity)
                    //TODO: send an email
                    return! text "Sending a password reset e-mail is not implemented yet :(" nxt ctx
                })
        ]
        routeCi "/authentication/login" >=> choose [ 
            GET  >=> csrfHtmlView (loginPage googleRecaptchSubmitButton [])
            POST 
                >=> requiresCsrfToken (text "CSRF token validation failed...")
                >=> requiresRecaptchaToken (csrfHtmlView (loginPage googleRecaptchSubmitButton [ "Google recaptcha is niet geldig" ]))
                >=> (fun nxt ctx -> task {
                    let! loginParams = ctx.BindFormAsync<LoginForm>()
                    let! authenticatedUserResult = system.AuthenticateUser (loginParams.UserName, loginParams.Password)

                    match authenticatedUserResult with
                    | Ok authenticatedUser ->
                        let claims = [ 
                            new Claim (JwtRegisteredClaimNames.Sub, string authenticatedUser.UserId) 
                            new Claim (JwtRegisteredClaimNames.Email, authenticatedUser.EmailAddress)
                            new Claim ("use2fac", if authenticatedUser.UseTwoFac then "1" else "0")
                        ]
                        let validatedUsernamePasswordToken = system.GenerateUsernamePasswordToken (claims, usernamePasswordTokenValidity)
                        return! redirectTo false (sprintf "/2fac?Token=%s" validatedUsernamePasswordToken) nxt ctx
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
            POST >=> (fun nxt ctx -> task {
                let! postParams = ctx.BindFormAsync<RecommendTwoFactorAuthenticationForm>()
                let requestParams = ctx.BindQueryString<TokenRequestParams>()
                match system.ValidateUsernamePasswordToken requestParams.Token with
                | Some identity ->
                    match postParams.SubmitButton with
                    | "UseTwoFac" ->
                        return! redirectTo false (sprintf "/authentication/enable2fac?Token=%s" requestParams.Token) nxt ctx
                    | _ ->
                        match! system.GetUser (identity.FindFirstValue (JwtRegisteredClaimNames.Sub) |> Guid.Parse) with
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
                    let secret = Encryption.generateRandomString(20)
                    let recoveryCodes = [
                        Encryption.generateRandomString(8)
                        Encryption.generateRandomString(8)
                        Encryption.generateRandomString(8)
                        Encryption.generateRandomString(8)
                    ]

                    //Making sure the secret doesn't go over the wire twice.
                    do sharedSecretCache.Set(requestParams.Token, (secret, recoveryCodes))

                    let tfa = new TwoFactorAuthenticator()
                    let setupInfo = tfa.GenerateSetupCode("Melior app", userEmailAddress, secret, false, 4)
                    let qrCodeImageUrl = setupInfo.QrCodeSetupImageUrl; //  assigning the Qr code information + URL to a base64 encoded string
                    let manualEntrySetupCode = setupInfo.ManualEntryKey; // show the Manual Entry Key for the users that don't have app or phone

                    return! csrfHtmlView (initializeTwoFacPage qrCodeImageUrl manualEntrySetupCode []) nxt ctx
                | None ->
                    return! htmlView usernamePasswordTokenExpiredPage nxt ctx
            })
            POST >=> (fun nxt ctx -> task {
                let! postParams = ctx.BindFormAsync<TwoFacLoginForm>()
                let requestParams = ctx.BindQueryString<TokenRequestParams>()
                match system.ValidateUsernamePasswordToken requestParams.Token with 
                | Some identity ->
                    let tfa = new TwoFactorAuthenticator();
                    let (secret, recoveryCodes): string * string list = defaultArg (sharedSecretCache.TryRetrieve(requestParams.Token)) ("", [])
                    match tfa.ValidateTwoFactorPIN(secret, postParams.VerificationCode, TimeSpan.FromMinutes(3.0)) with
                    | true ->
                        match! system.GetUser (identity.FindFirstValue (JwtRegisteredClaimNames.Sub) |> Guid.Parse) with
                        | Some currentUser ->
                            let updateTwoFac = {
                                UserId = currentUser.UserId
                                UseTwoFac = true
                                TwoFacSecret = secret
                                EmailAddress = currentUser.EmailAddress
                                RecoveryCodes = recoveryCodes
                            }
                            do! system.UpdateTwoFacAuthentication updateTwoFac
                            //TODO: send email for recoveryCodes
                            let claimsPrincipal = toClaimsPrincipal currentUser
                            //TODO: double check if everything works properly.
                            do! ctx.SignInAsync(loginAuthSchema, claimsPrincipal)
                            return! htmlView twoFacSucceeded nxt ctx
                        | None ->
                            //This should never occur really... But meh :)
                            return! htmlView usernamePasswordTokenExpiredPage nxt ctx
                    | false ->
                        let userEmailAddress = identity.FindFirstValue(JwtRegisteredClaimNames.Email)
                        do sharedSecretCache.Set(requestParams.Token, (secret, recoveryCodes))

                        let tfa = new TwoFactorAuthenticator()
                        let setupInfo = tfa.GenerateSetupCode("Melior app", userEmailAddress, secret, false, 4)
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
                            return! csrfHtmlView (twoFacPage []) nxt ctx
                        | _ ->
                            return! redirectTo false (sprintf "/authentication/recommend2fac?token=%s" requestParams.Token) nxt ctx
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
                        let userId = identity.FindFirstValue (JwtRegisteredClaimNames.Sub) |> Guid.Parse
                        match! system.GetUser userId with
                        | Some currentUser ->
                            match! system.ValidateTwoFactorPIN(currentUser.UserId, postParams.VerificationCode) with
                            | true ->
                                let claimsPrincipal = toClaimsPrincipal currentUser
                                //TODO: double check if everything works properly.
                                do! ctx.SignInAsync(loginAuthSchema, claimsPrincipal)
                                return! redirectTo false "/" nxt ctx
                            | false ->
                                do! system.AddFailedTwoFacAttempt { UserId = userId; Timestamp = DateTimeOffset.UtcNow }
                                let! nbAttempts = system.GetNbFailedTwoFacAttempts (userId, DateTimeOffset.UtcNow.Subtract(TimeSpan.FromMinutes(15.0)))
                                if nbAttempts > 5 then
                                    return! htmlView tooManyFailedTwoFacAttempts nxt ctx
                                else
                                    return! csrfHtmlView (twoFacPage [ "De ingegeven code was niet geldig. Gelieve opnieuw te proberen." ]) nxt ctx
                        | None ->
                            //This should never occur really... But meh :)
                            return! htmlView usernamePasswordTokenExpiredPage nxt ctx
                    | None ->
                        return! htmlView usernamePasswordTokenExpiredPage nxt ctx                        
                })
        ]
        routeCi "/authentication/accessDenied" >=> 
            GET >=> htmlView accessDeniedPage
        routeCi "/authentication/logout" >=> (fun nxt ctx -> task {
            //signout async will do the redirect, so just return with an earlyReturn
            do! ctx.SignOutAsync(CookieAuthenticationDefaults.AuthenticationScheme)
            return! earlyReturn ctx
        })
    ]