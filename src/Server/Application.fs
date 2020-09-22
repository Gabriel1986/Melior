namespace Server

open System
open System.Net.Http
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.AspNetCore.Authentication.Cookies
open FSharp.Control.Tasks
open Giraffe
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Serilog

open Server.Remoting
open Server.Blueprint.Behavior

module Application =
    let errorHandler (ex: System.Exception) (routeInfo: RouteInfo<HttpContext>) =
        // do some logging
        let logger = Log.Logger
        logger.Error("ErrorOutput at {Path} on method {Method}", routeInfo.path, routeInfo.methodName)
        logger.Error(ex, "Exception")
        Ignore

    let mustBeLoggedIn : HttpHandler =
        requiresAuthentication (challenge CookieAuthenticationDefaults.AuthenticationScheme) 

    let createEnvironment (config: IConfiguration) =
        let authenticationSystem = Authentication.AuthenticationSystem.build config
        let mediaSystem = Media.MediaSystem.build config
        let buildingSystem = Buildings.BuildingSystem.build config
        let professionalSyndicSystem = ProfessionalSyndics.ProfessionalSyndicSystem.build config
        let lotSystem = Lots.LotSystem.build config
        let organizationSystem = Organizations.OrganizationSystem.build config
        let ownerSystem = Owners.OwnerSystem.build config
        let contractSystem = Contracts.ContractSystem.build config
        let financialSystem = Financial.FinancialSystem.build config

        {
            new IEnv with
                member _.AuthenticationSystem = authenticationSystem
                member _.MediaSystem = mediaSystem
                member _.BuildingSystem = buildingSystem
                member _.ProfessionalSyndicSystem = professionalSyndicSystem
                member _.LotSystem = lotSystem
                member _.OrganizationSystem = organizationSystem
                member _.OwnerSystem = ownerSystem
                member _.ContractSystem = contractSystem
                member _.FinancialSystem = financialSystem
        }

    let build (config: IConfiguration) next ctx =
        let environment = createEnvironment config

        choose [
            environment.AuthenticationSystem.HttpHandler
            mustBeLoggedIn >=> choose [
                route "/" >=> (fun next ctx -> 
                    #if DEBUG
                        //The webpack dev server will host the file -> go fetch
                        task {
                            let client = new HttpClient()
                            //Used 127.0.0.1 here in stead of localhost because it was failing on linux.
                            //8080 is the port the webpack-dev-server runs under.
                            let! response = client.GetAsync("http://127.0.0.1:8092/main.html")
                            let! content = response.Content.ReadAsStreamAsync()
                            return! ctx.WriteStreamAsync true content None None
                        }
                    #else
                        //Webpack already compiled operator.html for us -> serve it
                        htmlFile "main.html" next ctx
                    #endif
                )
                
                environment.MediaSystem.HttpHandler

                Remoting.createApi()
                |> Remoting.withRouteBuilder Shared.Remoting.routeBuilder
                |> Remoting.fromContext (meliorBff environment)
                |> Remoting.withDiagnosticsLogger (printfn "%s")
                |> Remoting.withErrorHandler (errorHandler)
                |> Remoting.buildHttpHandler
            ]
        ] next ctx