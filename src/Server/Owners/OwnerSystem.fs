module Server.Owners.OwnerSystem

open Microsoft.Extensions.Configuration
open Server.AppSettings
open Server.Blueprint.Behavior.Owners
open Server.Library
open Server.LibraryExtensions

let build (config: IConfiguration): IOwnerSystem =
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.ConnectionString
    let store = Storage.makeStorage conn
    {
        new IOwnerSystem with
            member _.CreateOwner msg = Workflow.createOwner store msg
            member _.UpdateOwner msg = Workflow.updateOwner store msg
            member _.DeleteOwner msg = Workflow.deleteOwner store msg
            member _.GetOwner msg = async {
                match! Query.getOwner conn msg.Payload with
                | Some owner when msg.CurrentUser.HasAccessToBuilding owner.BuildingId ->
                    return Some owner
                | _ ->
                    return None
            }
            member _.GetOwners msg = 
                if msg.CurrentUser.HasAccessToBuilding msg.Payload.BuildingId 
                then
                    Query.getOwners conn msg.Payload
                else
                    Async.lift []
    }