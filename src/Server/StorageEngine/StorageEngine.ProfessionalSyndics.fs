module Server.StorageEngine.ProfessionalSyndicStorage

open Npgsql.FSharp
open Shared.Write
open Server.Library
open Server.PostgreSQL
open Server.Blueprint.Data.Storage

let private paramsFor (validated: ValidatedProfessionalSyndic) = [
    "@OrganizationId" , Sql.uuid validated.Organization.OrganizationId
]

let transformEventToSql (msg: Message<ProfessionalSyndicEvent>) =
    match msg.Payload with
    | ProfessionalSyndicEvent.ProfessionalSyndicEvent event ->
        match event with
        | CUDEvent.Created validated ->
            [
                yield! 
                    OrganizationStorage.transformEventToSql (
                        validated.Organization
                        |> OrganizationEvent.OrganizationWasCreated
                        |> inMsg msg
                    )
            
                "INSERT INTO ProfessionalSyndics (OrganizationId) VALUES (@OrganizationId)"
                , [ paramsFor validated ]
            ]
        | CUDEvent.Updated validated ->
            [
                yield! 
                    OrganizationStorage.transformEventToSql (
                        validated.Organization
                        |> OrganizationEvent.OrganizationWasUpdated
                        |> inMsg msg
                    )
            ]
        | CUDEvent.Deleted professionalSyndicId ->
            [
                yield! 
                    OrganizationStorage.transformEventToSql (
                        (None, professionalSyndicId)
                        |> OrganizationEvent.OrganizationWasDeleted
                        |> inMsg msg
                    )

                """
                    UPDATE ProfessionalSyndics
                    SET IsActive = FALSE
                    WHERE OrganizationId = @OrganizationId
                """
                , [[ "@OrganizationId", Sql.uuid professionalSyndicId ]]
            ]