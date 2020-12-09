module Server.ProfessionalSyndics.ProfessionalSyndicSystem

open Microsoft.Extensions.Configuration
open Server.Blueprint.Behavior.ProfessionalSyndics
open Server.Blueprint.Behavior.Storage
open Server.AppSettings
open Server.Library
open Server.LibraryExtensions
open Shared.Read

let build (config: IConfiguration) (store: IStorageEngine): IProfessionalSyndicSystem =
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection
    {
        new IProfessionalSyndicSystem with
            member _.CreateProfessionalSyndic msg = Workflow.createProfessionalSyndic store msg
            member _.UpdateProfessionalSyndic msg = Workflow.updateProfessionalSyndic store msg
            member _.DeleteProfessionalSyndic msg = Workflow.deleteProfessionalSyndic store msg
            member _.GetProfessionalSyndic msg = 
                if msg.CurrentUser.HasAdminAccessToProfessionalSyndic msg.Payload
                then Query.getProfessionalSyndic conn msg.Payload
                else Async.lift None
            member _.GetProfessionalSyndics msg =
                if msg.CurrentUser.IsSysAdmin () 
                then
                    match msg.Payload with
                    | Some organizationIds -> Query.getProfessionalSyndicsByIds conn organizationIds
                    | None -> Query.getProfessionalSyndics conn ()
                else
                    let accessibleOrganizationIds = msg.CurrentUser.Roles |> List.choose (fun role -> match role with | ProfessionalSyndicRole (orgId, _) -> Some orgId | _ -> None)

                    let filteredAccessibleOrganizationIds =
                        match msg.Payload with
                        | Some organizationIds ->
                            (organizationIds |> Set.ofList)
                            |> Set.intersect (accessibleOrganizationIds |> Set.ofList)
                            |> Set.toList
                        | None ->
                            accessibleOrganizationIds
                    Query.getProfessionalSyndicsByIds conn filteredAccessibleOrganizationIds
    }