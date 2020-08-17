module Server.Organizations.OrganizationSystem

open Microsoft.Extensions.Configuration
open Server.AppSettings
open Server.Blueprint.Behavior.Organizations
open Server.Library
open Server.LibraryExtensions

let build (config: IConfiguration): IOrganizationSystem =
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection
    let store = Storage.makeStorage conn
    {
        new IOrganizationSystem with
            member _.CreateOrganization msg = Workflow.createOrganization store msg
            member _.UpdateOrganization msg = Workflow.updateOrganization store msg
            member _.DeleteOrganization msg = Workflow.deleteOrganization store msg
            member _.CreateContactPerson msg = Workflow.createContactPerson store msg
            member _.UpdateContactPerson msg = Workflow.updateContactPerson store msg
            member _.DeleteContactPerson msg = Workflow.deleteContactPerson store msg
            member _.CreateOrganizationType msg = Workflow.createOrganizationType store msg
            member _.UpdateOrganizationType msg = Workflow.updateOrganizationType store msg
            member _.DeleteOrganizationType msg = Workflow.deleteOrganizationType store msg
            member _.GetOrganization msg = async {
                match! Query.getOrganization conn msg.Payload with
                | Some org when org.BuildingId.IsSome && msg.CurrentUser.HasAccessToBuilding org.BuildingId.Value ->
                    return Some org
                | _ ->
                    return None
            }
            member _.GetOrganizations msg =
                if msg.CurrentUser.HasAccessToBuilding msg.Payload.BuildingId 
                then
                    Query.getOrganizations conn msg.Payload
                else
                    Async.lift []
            member _.VerifyVatNumber vatNumber = ViesService.verifyVatNumber settings vatNumber
            member _.GetOrganizationTypes () = Query.getOrganizationTypes conn () |> Async.map (Seq.map (fun kvp -> kvp.Value) >> List.ofSeq)
    }