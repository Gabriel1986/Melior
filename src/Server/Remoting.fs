﻿module Server.Remoting
    open System
    open Microsoft.AspNetCore.Http

    open Server.AppSettings
    open Shared.Remoting
    open Server.Library
    open Microsoft.Extensions.Configuration
    open Server.Blueprint.Behavior
    open System.IdentityModel.Tokens.Jwt

    let meliorBff (config: IConfiguration) (environment: IEnv) (ctx: HttpContext): RemotingApi = 
        let settings = config.Get<AppSettings>()

        let createMsg payload: Message<'T> = {
            CreatedAt = DateTimeOffset.Now
            Context = ctx
            Payload = payload
        } 
        let connectionString = settings.Database.ConnectionString

        {
            GetCurrentUser = fun _ ->
                environment.AuthenticationSystem.GetUser (Guid.Parse (ctx.User.FindFirstValue(JwtRegisteredClaimNames.Sub)))
                |> Async.map (function | Some user -> user | None -> failwithf "This should not occur... Failed to fetch current user...")
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
                Server.Organizations.ViesService.verifyVatNumber settings vatNumber
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