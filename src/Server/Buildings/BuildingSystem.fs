module Server.Buildings.BuildingSystem

open Microsoft.Extensions.Configuration
open Server.AppSettings
open Server.Blueprint.Behavior.Buildings
open Server.LibraryExtensions
open Shared.Library

let build (config: IConfiguration): IBuildingSystem =
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection
    let store = Storage.makeStorage conn
    {
        new IBuildingSystem with
            member _.CreateBuilding msg = Workflow.createBuilding store msg
            member _.UpdateBuilding msg = Workflow.updateBuilding store msg
            member _.DeleteBuilding msg = Workflow.deleteBuilding store msg
            member _.UpdateBuildingSyndic msg = Workflow.updateBuildingSyndic store msg
            member _.UpdateBuildingConcierge msg = Workflow.updateBuildingConcierge store msg
            member _.GetBuilding msg = 
                if msg.CurrentUser.HasAccessToBuilding msg.Payload 
                then Query.getBuilding conn msg.Payload
                else Async.lift None
            member _.GetBuildings msg = 
                if msg.CurrentUser.IsSysAdmin ()
                then
                    Query.getAllBuildings conn ()
                else
                    let accessibleBuildingIds = msg.CurrentUser.Roles |> List.collect (fun role -> role.BuildingIds ())
                    Query.getBuildingsByIds conn accessibleBuildingIds
    }