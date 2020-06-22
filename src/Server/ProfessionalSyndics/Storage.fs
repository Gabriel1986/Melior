module Server.ProfessionalSyndics.Storage

open Npgsql.FSharp
open Server.PostgreSQL
open Shared.Write

let private paramsFor (validated: ValidatedProfessionalSyndic) =
    [
        "@OrganizationId"  , Sql.uuid validated.Organization.OrganizationId
    ]

let createProfessionalSyndic (connectionString: string) (validated: ValidatedProfessionalSyndic) =
    Sql.connect connectionString
    |> Sql.writeBatchAsync [
        Server.Organizations.Storage.createQuery, 
            Server.Organizations.Storage.paramsFor validated.Organization

        """
            INSERT INTO ProfessionalSyndics (
                OrganizationId
            ) VALUES (
                @OrganizationId
            )
        """, paramsFor validated
    ]

let updateProfessionalSyndic (connectionString: string) (validated: ValidatedProfessionalSyndic) =
    Sql.connect connectionString
    |> Sql.writeBatchAsync [
        Server.Organizations.Storage.updateQuery,
            Server.Organizations.Storage.paramsFor validated.Organization
    ]

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