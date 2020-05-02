namespace Server

open System
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open Giraffe
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Serilog

open Shared.Remoting
open Server.Library
open Shared.Domain
open FSharp.Control.Tasks
open System.Net.Http
open System.Threading.Tasks

module Application =
    let meliorBff (ctx: HttpContext): RemotingApi = 
        let createMsg payload: Message<'T> = {
            CreatedAt = DateTimeOffset.Now
            Context = ctx
            Payload = payload
        } 
        let connectionString = "TODO"
        let buildingStorage = Buildings.Storage.buildStorage connectionString

        {
            GetCurrentUser = fun _ -> async {
                do! Async.AwaitTask (Task.Delay (1000))
                return {
                    UserId = Guid.NewGuid()
                    EmailAddress = "test@melior.be"
                    DisplayName = "Testy de tester"
                    PersonId = Guid.NewGuid()
                    Role = Role.SysAdmin
                    BuildingIds = []
                }
            }
            CreateBuilding = fun building -> 
                createMsg building
                |> Server.Buildings.Workflow.createBuilding buildingStorage
            UpdateBuilding = fun building ->
                createMsg building
                |> Server.Buildings.Workflow.updateBuilding buildingStorage
            DeleteBuilding = fun buildingId ->
                createMsg buildingId
                |> Server.Buildings.Workflow.deleteBuilding buildingStorage
            GetBuilding = fun buildingId ->
                createMsg buildingId
                |> Server.Buildings.Query.getBuilding connectionString
            GetBuildings = fun () ->
                createMsg ()
                |> Server.Buildings.Query.getBuildings connectionString
            GetLotsForBuilding = fun buildingId ->
                createMsg buildingId
                |> Server.Lots.Query.getLotsForBuilding connectionString
            GetResidentsForBuilding = fun buildingId ->
                createMsg buildingId
                |> Server.Residents.Query.getResidentsForBuilding connectionString
            DeleteResident = fun residentId ->
                createMsg residentId
                |> Server.Residents.Workflow.deleteResident connectionString
            GetResidents = fun () ->
                createMsg ()
                |> Server.Residents.Query.getResidents connectionString
            GetResident = fun residentId ->
                createMsg residentId
                |> Server.Residents.Query.getResident connectionString
            DeleteLot = fun lotId ->
                createMsg lotId
                |> Server.Lots.Workflow.deleteLot connectionString
            GetLots = fun () ->
                createMsg ()
                |> Server.Lots.Query.getLots connectionString
            GetLot = fun lotId ->
                createMsg lotId
                |> Server.Lots.Query.getLot connectionString
        }


    let errorHandler (ex: System.Exception) (routeInfo: RouteInfo<HttpContext>) =
        // do some logging
        let logger = Log.Logger
        logger.Error("ErrorOutput at {Path} on method {Method}", routeInfo.path, routeInfo.methodName)
        logger.Error(ex, "Exception")
        Ignore

    let build env next ctx =
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
            Remoting.createApi()
            |> Remoting.withRouteBuilder Shared.Remoting.routeBuilder
            |> Remoting.fromContext meliorBff
            |> Remoting.withDiagnosticsLogger (printfn "%s")
            |> Remoting.withErrorHandler errorHandler
            |> Remoting.buildHttpHandler
        ] next ctx