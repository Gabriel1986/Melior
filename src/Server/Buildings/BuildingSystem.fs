module Server.Buildings.BuildingSystem

open Microsoft.Extensions.Configuration
open Server.AppSettings
open Server.Blueprint.Behavior.Buildings
open Server.Blueprint.Behavior.Storage
open Server.LibraryExtensions
open Shared.Library

let build (config: IConfiguration) (storageEngine: IStorageEngine): IBuildingSystem =
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection
    {
        new IBuildingSystem with
            member _.CreateBuilding msg = Workflow.createBuilding storageEngine msg
            member _.UpdateBuilding msg = Workflow.updateBuilding storageEngine msg
            member _.DeleteBuilding msg = Workflow.deleteBuilding storageEngine msg
            member _.UpdateBuildingSyndic msg = Workflow.updateBuildingSyndic storageEngine msg
            member _.UpdateBuildingConcierge msg = Workflow.updateBuildingConcierge storageEngine msg
            member _.GetBuilding msg = 
                if msg.CurrentUser.HasAccessToBuilding msg.Payload 
                then Query.getBuilding conn msg.Payload
                else Async.lift None
            member _.GetBuildings msg = 
                if msg.CurrentUser.IsSysAdmin ()
                then
                    match msg.Payload with
                    | Some buildingIds ->
                        Query.getBuildingsByIds conn buildingIds
                    | None ->
                        Query.getAllBuildings conn ()
                else
                    let accessibleBuildingIds = msg.CurrentUser.Roles |> List.collect (fun role -> role.BuildingIds ())
                    let filteredAccessibleBuildingIds =
                        match msg.Payload with
                        | Some buildingIds ->
                            (buildingIds |> Set.ofList)
                            |> Set.intersect (accessibleBuildingIds |> Set.ofList)
                            |> Set.toList
                        | None ->
                            accessibleBuildingIds
                    Query.getBuildingsByIds conn filteredAccessibleBuildingIds
    }