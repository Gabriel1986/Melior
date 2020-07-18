module Server.Authentication.HttpHandler

open Microsoft.Extensions.Configuration
open System
open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.Antiforgery

open Server.AppSettings
open Server.Library

[<AutoOpen>]
module private Internals =
    let createMessage (ctx: HttpContext) (data: 'T) : Message<'T> = { 
        Context = ctx
        Payload = data
        CreatedAt = DateTimeOffset.Now
    }
    
let mediaHandler (config: IConfiguration) =
    let settings = config.Get<AppSettings>()
    let connectionString = settings.Database.ConnectionString
    choose [
        //TODO: https://docs.microsoft.com/en-us/aspnet/core/security/authentication/cookie?view=aspnetcore-3.1
        routeCi "/authentication/login" >=> choose [ 
            GET  >=> csrfHtmlView Login.htmlView
            POST >=> requiresCsrfToken (text "CSRF token validation failed...") >=> text "oh hi there ;)" 
        ]
        routeCi "/authentication/2FactAuth" >=> choose [
            GET  >=> csrfHtmlView Login.htmlView
            POST >=> requiresCsrfToken (text "CSRF token validation failed...") >=> text "oh hi there ;)"
        ]
        routeCi "/authentication/logout" >=> choose [
            GET >=> csrfHtmlView Logout.htmlView
            POST >=> requiresCsrfToken (text "CSRF token validation failed...") >=> text "oh hi there ;)"
        ]
    ]