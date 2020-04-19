namespace Server

open System
open Microsoft.AspNetCore.Http
open Giraffe
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Serilog

open Shared.Remoting
open Server.Library

module Application =
    let meliorBff (ctx: HttpContext): RemotingApi = 
        let createMsg payload: Message<'T> = {
            CreatedAt = DateTimeOffset.Now
            Context = ctx
            Payload = payload
        } 
        {
            CreateBuilding = fun building -> 
                createMsg building
                |> Server.Buildings.Workflow.createBuilding connectionString
            UpdateBuilding = fun building ->
                createMsg building
                |> Server.Buildings.Workflow.updateBuilding connectionString
            DeleteBuilding = fun buildingId ->
                createMsg buildingId
                |> Server.Buildings.Workflow.deleteBuilding connectionString
            GetBuilding = fun buildingId ->
                createMsg buildingId
                |> Server.Buildings.Query.getBuilding connectionString
            GetBuildings = fun () ->
                createMsg ()
                |> Server.Buildings.Query.getBuildings connectionString
        }


    let errorHandler (ex: System.Exception) (routeInfo: RouteInfo<HttpContext>) =
        // do some logging
        let logger = Log.Logger
        logger.Error("ErrorOutput at {Path} on method {Method}", routeInfo.path, routeInfo.methodName)
        logger.Error(ex, "Exception")
        Ignore

    let build env next ctx =
        choose [
            route "/"
                >=> (fun next ctx -> htmlFile "main.html" next ctx)
            Remoting.createApi()
            |> Remoting.withRouteBuilder (sprintf "/api/%s/%s")
            |> Remoting.fromContext meliorBff
            |> Remoting.withDiagnosticsLogger (printfn "%s")
            |> Remoting.withErrorHandler errorHandler
            |> Remoting.buildHttpHandler
        ] next ctx