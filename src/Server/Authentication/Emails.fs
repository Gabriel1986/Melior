module Server.Authentication.Emails

open Giraffe.GiraffeViewEngine
open Shared.Read

let inEmailLayout (header: XmlNode list) (body: XmlNode list) (footer: XmlNode list) =
    table [ 
        attr "border" "0"
        attr "cellpadding" "0"
        attr "cellspacing" "0"
        _height "100%"
        _width "100%"
        _id "bodyTable" 
    ] [
        tr [] [
            td [ 
                attr "align" "center"
                attr "valign" "top" 
            ] [
                table [ 
                    attr "border" "0"
                    attr "cellpadding" "20"
                    attr "cellspacing" "0"
                    _width "600px"
                    _id "emailContainer" 
                ] [ 
                    if header.Length > 0 then
                        tr [] [ 
                            td [ 
                                attr "align" "center"
                                attr "valign" "top" 
                            ] [
                                table [ 
                                    attr "border" "0"
                                    attr "cellpadding" "20"
                                    attr "cellspacing" "0"
                                    _width "100%"
                                    _id "emailHeader" 
                                ] [ 
                                    tr [] [ 
                                        td [ 
                                            attr "align" "center"
                                            attr "valign" "top" 
                                    ] header ] 
                                ] 
                            ] 
                        ]
                    if body.Length > 0 then
                        tr [] [ 
                            td [ 
                                attr "align" "center"
                                attr "valign" "top" 
                            ] [
                                table [ 
                                    attr "border" "0"
                                    attr "cellpadding" "20"
                                    attr "cellspacing" "0"
                                    _width "100%"
                                    _id "emailBody"
                                ] [ 
                                    tr [] [ 
                                        td [ 
                                            attr "align" "center"
                                            attr "valign" "top" 
                                    ] body ] 
                                ] 
                            ] 
                        ]
                    if footer.Length > 0 then
                        tr [] [ 
                            td [ 
                                attr "align" "center"
                                attr "valign" "top" 
                            ] [
                                table [ 
                                    attr "border" "0"
                                    attr "cellpadding" "20"
                                    attr "cellspacing" "0"
                                    _width "100%"
                                    _id "emailFooter"
                                ] [ 
                                    tr [] [ 
                                        td [ 
                                            attr "align" "center"
                                            attr "valign" "top" 
                                    ] footer ]
                                ] 
                            ] 
                        ]
                    ]
                ]
            ]
        ]

let inSimpleEmailLayout (body: XmlNode list) =
    inEmailLayout [] body []


let inEmailRow content = 
    tr [] [
        td [
            attr "valign" "top"
            attr "style" "font-family:Arial,sans-serif;font-size: 14px"; 
        ] [ content ]
    ]

let resetPasswordContent (linkToResetPassword: string): XmlNode =
    tr [] [ 
        td [ 
            _width "100%"
            _style "padding:24px 36px 24px 36px;min-width:100%;"
        ] [ 
            table [ 
                attr "bgcolor" "#f9f9f9"
                attr "cellpadding" "0"
                attr "cellspacing" "0"
                attr "width" "100%"
                attr "style" "background-color:#f9f9f9;border-radius:6px;border:1px solid #ededed;min-width:100%"
            ] [
                tr [] [ 
                    td [ 
                        attr "align" "center"
                        attr "style" "padding:20px 0 16px 0"
                    ] [ 
                        table [ 
                            attr "cellpadding" "0"
                            attr "cellspacing" "0" 
                        ] [
                            tr [] [
                                td [
                                    attr "align" "center"
                                    attr "valign" "top"
                                    attr "style" "font-family:Arial,sans-serif;font-size:14px"
                                ] [
                                    a [ 
                                        attr "href" linkToResetPassword
                                        attr "style" "background-color:#30aadd;border-radius:6px;color:#ffffff;display:inline-block;font-family:Arial,sans-serif;font-size:16px;font-weight:600;line-height:50px;text-align:center;text-decoration:none;width:286px"
                                        attr "target" "_blank"
                                    ] [ 
                                        str "Wachtwoord opnieuw instellen" 
                                    ]
                                ]
                            ]

                            tr [] [ 
                                td [ 
                                    attr "align" "center"
                                    attr "style" "color:#3bb3e6;font-family:Arial,sans-serif;font-size:11px;line-height:15px;padding:16px 38px 16px 38px;max-width:526px;word-break:break-all"
                                ] [ 
                                    a [
                                        attr "href" linkToResetPassword
                                        attr "target" "_blank"
                                    ] [ 
                                        str linkToResetPassword
                                    ]
                                ] 
                            ]
                            tr [] [ 
                                td [ 
                                    attr "align" "center"
                                    attr "style" "color:#777777"
                                ] [ 
                                    str "Werkt de knop niet? Knip en plak bovenstaande link in uw browser." 
                                ] 
                            ] 
                        ] 
                    ] 
                ] 
            ] 
        ]
    ]

let resetPasswordEmail (user: User, linkToResetPassword: string): XmlNode =
    [
        table [
            attr "border" "0"
            attr "cellpadding" "0"
            attr "cellspacing" "0"
            _width "100%"
        ] [
            h4 [ _style "margin:0;" ] [ str (sprintf "Hallo %s," user.DisplayName) ]
            |> inEmailRow

            p [ _style "margin:0;" ] [
                str "Bent u uw wachtwoord vergeten? Geen probleem! Klik op de knop hieronder om uw wachtwoord opnieuw in te stellen."
            ]
            |> inEmailRow

            resetPasswordContent linkToResetPassword

            p [ _style "margin:0;" ] [
                str "Indien u geen nieuw wachtwoord heeft aangevraagd, gelieve deze e-mail te negeren en niet op de link te klikken."
            ]
            |> inEmailRow

            p [ _style "margin:0;padding-top:10px" ] [
                str "Vriendelijke groeten,"
                br []
                str "Het SyndicusAssistent team"
            ]
            |> inEmailRow
        ]
        p [] [
        
        ]
    ]
    |> inSimpleEmailLayout