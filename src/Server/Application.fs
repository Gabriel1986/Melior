namespace Server

open System
open Microsoft.AspNetCore.Http
open Giraffe
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Serilog

open Shared.Remoting
open Server.Library
open Shared.Read
open FSharp.Control.Tasks
open System.Net.Http
open Server.AppSettings

module Application =
    let meliorBff (settings: AppSettings) (ctx: HttpContext): RemotingApi = 
        let createMsg payload: Message<'T> = {
            CreatedAt = DateTimeOffset.Now
            Context = ctx
            Payload = payload
        } 
        let connectionString = settings.Database.ConnectionString

        {
            GetCurrentUser = fun _ -> async {
                let! buildings = Server.Buildings.Query.getBuildings connectionString ()

                return {
                    UserId = Guid.NewGuid()
                    EmailAddress = "test@melior.be"
                    DisplayName = "Testy de tester"
                    PersonId = Guid.NewGuid()
                    Role = Role.SysAdmin
                    BuildingIds = buildings |> List.map (fun b -> b.BuildingId)
                    PreferredLanguageCode = "nl-BE"
                }
            }
            CreateBuilding = fun building -> 
                createMsg building
                |> Server.Buildings.Workflow.createBuilding connectionString
            UpdateBuilding = fun building ->
                createMsg building
                |> Server.Buildings.Workflow.updateBuilding connectionString
            DeleteBuilding = fun buildingId ->
                createMsg buildingId
                |> Server.Buildings.Workflow.deleteBuilding connectionString
            UpdateBuildingSyndic = fun (buildingId, syndicId) ->
                createMsg (buildingId, syndicId)
                |> Server.Buildings.Workflow.updateBuildingSyndic connectionString
            UpdateBuildingConcierge = fun (buildingId, conciergeId) ->
                createMsg (buildingId, conciergeId)
                |> Server.Buildings.Workflow.updateBuildingConcierge connectionString
            GetBuilding = fun buildingId ->
                buildingId
                |> Server.Buildings.Query.getBuilding connectionString
            GetBuildings = fun () ->
                ()
                |> Server.Buildings.Query.getBuildings connectionString
            CreateOwner = fun owner ->
                createMsg owner
                |> Server.Owners.Workflow.createOwner connectionString
            UpdateOwner = fun owner ->
                createMsg owner
                |> Server.Owners.Workflow.updateOwner connectionString
            DeleteOwner = fun ownerId ->
                createMsg ownerId
                |> Server.Owners.Workflow.deleteOwner connectionString
            GetOwners = fun buildingFilter ->
                {| BuildingId = buildingFilter.BuildingId |}
                |> Server.Owners.Query.getOwners connectionString            
            GetOwner = fun ownerId ->
                ownerId
                |> Server.Owners.Query.getOwner connectionString
            CreateLot = fun lot ->
                createMsg lot
                |> Server.Lots.Workflow.createLot connectionString
            UpdateLot = fun lot ->
                createMsg lot
                |> Server.Lots.Workflow.updateLot connectionString
            DeleteLot = fun lotId ->
                createMsg lotId
                |> Server.Lots.Workflow.deleteLot connectionString
            GetLots = fun buildingFilter ->
                {| BuildingId = buildingFilter.BuildingId |}
                |> Server.Lots.Query.getLots connectionString
            GetLot = fun lotId ->
                lotId
                |> Server.Lots.Query.getLot connectionString
            CreateOrganization = fun org ->
                createMsg org
                |> Server.Organizations.Workflow.createOrganization connectionString
            UpdateOrganization = fun org ->
                createMsg org
                |> Server.Organizations.Workflow.updateOrganization connectionString
            DeleteOrganization = fun orgNr ->
                createMsg orgNr
                |> Server.Organizations.Workflow.deleteOrganization connectionString
            GetOrganizations = fun buildingFilter ->
                {| BuildingId = buildingFilter.BuildingId |}
                |> Server.Organizations.Query.getOrganizations connectionString
            GetOrganization = fun orgNr ->
                orgNr
                |> Server.Organizations.Query.getOrganization connectionString
            VerifyVatNumber = fun vatNumber ->                
                Server.Organizations.ViesService.verifyVatNumber vatNumber
            CreatePerson = fun pers ->
                createMsg pers
                |> Server.Persons.Workflow.createPerson connectionString
            UpdatePerson = fun pers ->
                createMsg pers
                |> Server.Persons.Workflow.updatePerson connectionString
            CreateProfessionalSyndic = fun syndic ->
                createMsg syndic
                |> Server.ProfessionalSyndics.Workflow.createProfessionalSyndic connectionString
            UpdateProfessionalSyndic = fun syndic ->
                createMsg syndic
                |> Server.ProfessionalSyndics.Workflow.updateProfessionalSyndic connectionString
            DeleteProfessionalSyndic = fun syndicId ->
                createMsg syndicId
                |> Server.ProfessionalSyndics.Workflow.deleteProfessionalSyndic connectionString
            GetProfessionalSyndics = fun _ ->
                ()
                |> Server.ProfessionalSyndics.Query.getProfessionalSyndics connectionString
            GetProfessionalSyndic = fun syndicId ->
                syndicId
                |> Server.ProfessionalSyndics.Query.getProfessionalSyndic connectionString
            GetOrganizationTypes = fun _ ->
                ()
                |> Server.Organizations.Query.getOrganizationTypes connectionString
                |> Async.map (fun dictionary -> dictionary.Values |> Seq.toList)
            CreateOrganizationType = fun orgType ->
                createMsg orgType
                |> Server.Organizations.Workflow.createOrganizationType connectionString
            UpdateOrganizationType = fun orgType ->
                createMsg orgType
                |> Server.Organizations.Workflow.updateOrganizationType connectionString
            DeleteOrganizationType = fun orgTypeId ->
                createMsg orgTypeId
                |> Server.Organizations.Workflow.deleteOrganizationType connectionString
        }


    let errorHandler (ex: System.Exception) (routeInfo: RouteInfo<HttpContext>) =
        // do some logging
        let logger = Log.Logger
        logger.Error("ErrorOutput at {Path} on method {Method}", routeInfo.path, routeInfo.methodName)
        logger.Error(ex, "Exception")
        Ignore

    let build (settings: AppSettings) next ctx =
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
            |> Remoting.fromContext (meliorBff settings)
            |> Remoting.withDiagnosticsLogger (printfn "%s")
            |> Remoting.withErrorHandler errorHandler
            |> Remoting.buildHttpHandler
        ] next ctx