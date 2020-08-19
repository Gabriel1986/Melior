module Server.Authentication.Pages

open Microsoft.AspNetCore.Antiforgery
open Giraffe.GiraffeViewEngine
open Giraffe.GiraffeViewEngine.Antiforgery

let masterPage (pageTitle : string) (content : XmlNode list) =
    html [] [
        head [] [
            title [] [ str pageTitle ]
            meta [ _charset "utf-8" ]
            meta [ _httpEquiv "X-UA-Compatible"; _content "IE=edge" ]
            meta [ _name "viewport"; _content "width=device-width, initial-scale=1, shrink-to-fit=no" ]
            link [ 
                _href "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.5.2/css/bootstrap.min.css"
                _rel "stylesheet"
            ]
            style [ ] [ str 
                """
                    .login-form {
                        max-width: 500px;
                        margin: 50px auto;
          	            font-size: 15px;
                    }
                """ 
            ]
        ]
        body [] [
            div [ _class "container-fluid" ] [
                div [ _class "login-form" ] [
                    h2 [ _class "text-center" ] [ str pageTitle ]
                    main [] content
                ]
            ]
            script [ _src "https://www.google.com/recaptcha/api.js" ] []
        ]
    ]

let validationHeader (validationErrors: string list) =
    match validationErrors with
    | [] -> []
    | errors ->
        [
            div [ _class "card-header" ] [
                yield! errors |> List.map (fun error -> div [] [ p [ _style "color: Red;" ] [ str error ] ])
            ]
        ]

let passwordTokenExpiredPage =
    [
        div [ _class "card" ] [
            div [ _class "card-body" ] [
                label [] [ 
                    str "Er is iets fout gelopen" 
                ]
                p [] [
                    str "De termijn waarin u uw wachtwoord opnieuw kon instellen is verlopen. U kan onderstaande link volgen om nogmaals te proberen een nieuw wachtwoord aan te vragen."
                ]
                a [ _href "/authentication/forgotPassword" ] [ 
                    str "Wachtwoord vergeten" 
                ]
            ]
        ]
    ]
    |> masterPage "Insteltermijn verlopen"

let usernamePasswordTokenExpiredPage =
    [
        div [ _class "card" ] [
            div [ _class "card-body" ] [
                label [] [ 
                    str "Er is iets fout gelopen" 
                ]
                p [] [
                    str "Uw sessie is verlopen, gelieve opnieuw aan te melden om opnieuw te proberen."
                ]
                a [ _href "/authentication/login" ] [ 
                    str "Naar login"
                ]
            ]
        ]
    ]
    |> masterPage "Insteltermijn verlopen"

let changePasswordPage (submitButton: string -> XmlNode) (validationErrors: string list) (antiForgeryToken: AntiforgeryTokenSet) =
    [
        div [ _class "card" ] [
            yield! validationHeader validationErrors
                
            yield div [ _class "card-body" ] [
                form [ _method "POST" ] [
                    antiforgeryInput antiForgeryToken
                    div [ _class "form-group" ] [
                        label [] [ str "Wachtwoord:" ]
                        input [ _name "NewPassword1"; _type "text"; _required; _class "form-control" ]
                    ]
                    div [ _class "form-group" ] [
                        label [] [ str "Wachtwoord herhalen:" ]
                        input [ _name "NewPassword2"; _type "text"; _required; _class "form-control" ]
                    ]
                    div [ _class "form-group" ] [
                        submitButton "Opnieuw instellen"
                    ]
                ]
            ]
        ]
    ]
    |> masterPage "Wachtwoord opnieuw instellen"

let forgotPasswordPage (submitButton: string -> XmlNode) (validationErrors: string list) (antiForgeryToken: AntiforgeryTokenSet) =
    [
        div [ _class "card" ] [
            yield! validationHeader validationErrors
            yield div [ _class "card-body" ] [
                form [ _method "POST" ] [
                    antiforgeryInput antiForgeryToken
                    div [] [
                        p [] [ str "U kan hier uw e-mail adres opnieuw ingeven." ]
                        p [] [ str "U zal een e-mail ontvangen waarin een link zit om uw wachtwoord opnieuw te kunnen instellen." ]
                    ]
                    div [ _class "form-group" ] [
                        label [] [ str "E-mail adres:" ]
                        input [ _name "UserName"; _type "text"; _required; _class "form-control" ]
                    ]
                    div [ _class "form-group" ] [
                        submitButton "E-mail versturen"
                    ]
                ]
            ]
        ]
    ]
    |> masterPage "Wachtwoord vergeten"

let loginPage (submitButton: string -> XmlNode) (validationErrors: string list) (antiForgeryToken: AntiforgeryTokenSet) =   
    [
        div [ _class "card" ] [
            yield! validationHeader validationErrors

            yield div [ _class "card-body" ] [
                form [ _method "POST" ] [
                    antiforgeryInput antiForgeryToken
                    div [ _class "form-group"] [
                        label [] [ str "E-mail adres:" ]
                        input [ _name "UserName"; _type "text"; _required; _class "form-control" ]
                    ]
                    div [ _class "form-group" ] [
                        label [] [ str "Wachtwoord:" ]
                        input [ _name "Password"; _type "password"; _required; _class "form-control" ]
                    ]
                    submitButton "Aanmelden"
                ]
            ]
            yield div [ _class "card-footer text-muted" ] [
                a [ _href "/authentication/forgotPassword" ] [ str "Wachtwoord vergeten" ]
            ]
        ]
    ] |> masterPage "Login"

let recommendTwoFacPage (antiForgeryToken: AntiforgeryTokenSet) =
    [
        div [ _class "card" ] [
            yield div [ _class "card-body" ] [
                form [ _method "POST" ] [
                    antiforgeryInput antiForgeryToken
                    div [ _class "form-group"] [
                        p [] [
                            str 
                                "Om uw gegevens te beveiligen raden we aan om een tweede identificatie-methode in te stellen (twee factor authenticatie).
Hiervoor dient u een authenticator app te installeren op uw smartphone (bvb. google authenticator, authy,...)."
                        ]
                    ]
                    div [] [
                        button [ _class "btn btn-danger"; _type "submit"; _name "SubmitButton"; _value "Continue" ] [
                            str "Doorgaan zonder instellen"
                        ]
                        str " "
                        button [ _class "btn btn-primary"; _type "submit"; _name "SubmitButton"; _value "UseTwoFac" ] [
                            str "Extra authenticatie Instellen"
                        ]
                    ]
                ]
            ]
        ]
    ] |> masterPage ""

let initializeTwoFacPage (qrCode: string) (manualEntrySetupCode: string) (validationErrors: string list) (antiForgeryToken: AntiforgeryTokenSet) = 
    [
        div [ _class "card" ] [
            yield! validationHeader validationErrors
            yield div [ _class "card-body" ] [
                form [ _method "POST" ] [
                    antiforgeryInput antiForgeryToken
                    div [ _style "text-align: center" ] [
                        label [] [ str "U kan deze code scannen in uw gekozen authenticator app:" ]
                        img [ _src qrCode; _alt "Uw browser blokkeert de QR code, gelieve de manuele instellingscode in uw app in te geven" ]
                    ]
                    div [ _class "form-group" ] [
                        p [] [ str (sprintf "Manuele instellingscode (indien de QR code niet werkt): %s" manualEntrySetupCode) ]
                    ]
                    div [ _class "form-group" ] [
                        label [] [ str "De code die uw app u toont (deze wijzigt elke 30 seconden):" ]
                        input [ _name "VerificationCode"; _type "text"; _required; _maxlength "6"; _class "form-control" ]
                    ]
                ]
            ]
        ]
    ] |> masterPage ""

let twoFacSucceeded =
    [
        div [ _class "card text-center" ] [
            h3 [] [
                str "Het instellen van de twee factor authenticatie is geslaagd"
            ]
            p [] [
                str "De applicatie zal u de volgende keer, na het aanmelden met gebruikersnaam en wachtwoord, om een code vragen die u op de authenticator app kan terugvinden."
            ]
            a [ _class "btn btn-success"; _href "/" ] [
                str "Naar de applicatie"
            ]
        ]
    ] |> masterPage ""

let twoFacPage (validationErrors: string list) (token: string) (antiForgeryToken: AntiforgeryTokenSet) =
    [
        div [ _class "card" ] [
            yield! validationHeader validationErrors
            yield div [ _class "card-body" ] [
                form [ _method "POST" ] [
                    antiforgeryInput antiForgeryToken
                    div [ _class "form-group" ] [
                        label [] [ str "Code die door de authenticator app getoond wordt:" ]
                        input [ _name "VerificationCode"; _type "text"; _required; _maxlength "6"; _class "form-control" ]
                    ]
                ]
            ]
            yield div [ _class "card-footer text-muted" ] [
                a [ _href (sprintf "/authentication/enter2facRecoveryCode?Token=%s" token) ] [ str "Herstelcode ingeven" ]
                //TODO: send recovery SMS (maybe)
            ]
        ]
    ] |> masterPage ""

let twoFacRecoveryCodePage (validationErrors: string list) (token: string) (antiForgeryToken: AntiforgeryTokenSet) =
    [
        div [ _class "card" ] [
            yield! validationHeader validationErrors
            yield div [ _class "card-body" ] [
                form [ _method "POST" ] [
                    antiforgeryInput antiForgeryToken
                    div [ _class "form-group" ] [
                        label [] [ str "1 van de 4 herstelcodes die u door ons bezorgd werd:" ]
                        input [ _name "RecoveryCode"; _type "text"; _required; _maxlength "8"; _class "form-control" ]
                        p [ _class "font-weight-light" ] [
                            str "Nota: de ingegeven herstelcode wordt onbruikbaar na validatie. U kan een set nieuwe codes aanvragen in 'Mijn profiel'"
                        ]
                    ]
                    div [] [
                        button [ _class "btn btn-primary"; _type "submit" ] [
                            str "Valideren"
                        ]
                    ]
                ]
            ]
            yield div [ _class "card-footer text-muted" ] [
                a [ _href (sprintf "/authentication/2fac?Token=%s" token) ] [
                    str "Terug naar de vorige stap"
                ]
            ]
        ]
    ] |> masterPage "Herstelcode"

let tooManyFailedTwoFacAttempts =
    [
        div [ _class "card" ] [
            div [ _class "card-body" ] [
                p [ _style "color: Red;" ] [
                    str "Uw account werd tijdelijk geblokeerd omwille van te veel mislukte pogingen. Gelieve het op een later moment opnieuw te proberen."
                ]
            ]
        ]
    ] |> masterPage "Toegang geweigerd"

let accessDeniedPage =
    [
        div [ _class "card" ] [
            div [ _class "card-body" ] [
                p [ _style "color: Red;" ] [
                    str "U heeft geen toegang tot deze pagina..."
                ]
            ]
        ]
    ] |> masterPage "Toegang geweigerd"