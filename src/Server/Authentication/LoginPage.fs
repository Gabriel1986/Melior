module Server.Authentication.Login

open Microsoft.AspNetCore.Antiforgery

open Giraffe.GiraffeViewEngine
open Giraffe.GiraffeViewEngine.Antiforgery

let htmlView (antiForgeryToken: AntiforgeryTokenSet) =
    form [ _method "POST" ] [
        antiforgeryInput antiForgeryToken
        div [] [
            str "TODO..."
        ]
    ]