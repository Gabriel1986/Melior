namespace Server

open System
open Microsoft.AspNetCore.Http
open Giraffe
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Serilog

open FSharp.Control.Tasks
open System.Net.Http
open Server.Media.HttpHandler
open Server.Remoting
open Microsoft.Extensions.Configuration

module Application =
    let errorHandler (ex: System.Exception) (routeInfo: RouteInfo<HttpContext>) =
        // do some logging
        let logger = Log.Logger
        logger.Error("ErrorOutput at {Path} on method {Method}", routeInfo.path, routeInfo.methodName)
        logger.Error(ex, "Exception")
        Ignore

    let build (config: IConfiguration) next ctx =
        choose [
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
            mediaHandler config
            Remoting.createApi()
            |> Remoting.withRouteBuilder Shared.Remoting.routeBuilder
            |> Remoting.fromContext (meliorBff config)
            |> Remoting.withDiagnosticsLogger (printfn "%s")
            |> Remoting.withErrorHandler errorHandler
            |> Remoting.buildHttpHandler
        ] next ctx