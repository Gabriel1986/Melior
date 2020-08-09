module Server.ProfessionalSyndics.Storage

open System
open Npgsql.FSharp
open Server.PostgreSQL
open Shared.Write

type IProfessionalSyndicStorage =
    abstract CreateProfessionalSyndic: ValidatedProfessionalSyndic -> Async<unit>
    abstract UpdateProfessionalSyndic: ValidatedProfessionalSyndic -> Async<int>
    abstract DeleteProfessionalSyndic: professionalSyndicId: Guid  -> Async<int>

let private paramsFor (validated: ValidatedProfessionalSyndic) =
    [
        "@OrganizationId"  , Sql.uuid validated.Organization.OrganizationId
    ]

let createProfessionalSyndic (connectionString: string) (validated: ValidatedProfessionalSyndic) =
    Sql.connect connectionString
    |> Sql.writeBatchAsync [
        Server.Organizations.Storage.createQuery, 
            Server.Organizations.Storage.paramsFor validated.Organization

        "INSERT INTO ProfessionalSyndics (OrganizationId) VALUES (@OrganizationId)", 
        paramsFor validated
    ]
    |> Async.Ignore

let updateProfessionalSyndic (connectionString: string) (validated: ValidatedProfessionalSyndic) =
    Sql.connect connectionString
    |> Sql.query Server.Organizations.Storage.updateQuery
    |> Sql.parameters (Server.Organizations.Storage.paramsFor validated.Organization)
    |> Sql.writeAsync

let deleteProfessionalSyndic connectionString organizationId =
    Sql.connect connectionString
    |> Sql.query
        """
            UPDATE ProfessionalSyndics
            SET IsActive = FALSE
            WHERE OrganizationId = @OrganizationId
        """
    |> Sql.parameters [ "@OrganizationId", Sql.uuid organizationId ]
    |> Sql.writeAsync

let makeStorage conn = {
    new IProfessionalSyndicStorage with
        member _.CreateProfessionalSyndic syndic = createProfessionalSyndic conn syndic
        member _.UpdateProfessionalSyndic syndic = updateProfessionalSyndic conn syndic
        member _.DeleteProfessionalSyndic syndicId = deleteProfessionalSyndic conn syndicId
}