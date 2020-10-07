module Server.ProfessionalSyndics.Query

open System
open Npgsql.FSharp
open Server.PostgreSQL
open Server.PostgreSQL.Sql
open Shared.Read
open Server.Addresses.Library
open Server.Library

[<AutoOpen>]
module private Readers =
    type ProfessionalSyndicDbModel = {
        OrganizationId: Guid
        BuildingId: Guid option
    }

    let readProfessionalSyndic (reader: CaseInsensitiveRowReader): ProfessionalSyndicDbModel = {
        OrganizationId = reader.uuid "OrganizationId"
        BuildingId = reader.uuidOrNone "BuildingId"
    }

    let readProfessionalSyndics (reader: CaseInsensitiveRowReader): ProfessionalSyndicListItem = {
        OrganizationId = reader.uuid "OrganizationId"
        Name = reader.string "Name"
        Address = 
            let addrString = reader.stringOrNone "Address"
            match addrString with
            | Some str -> Address.fromJson str |> Option.fromResult |> Option.defaultValue (Address.Init ())
            | None -> Address.Init ()
        MainEmailAddress = reader.stringOrNone "MainEmailAddress"
        MainTelephoneNumber = reader.stringOrNone "MainTelephoneNumber"
    }

//Can probably be simplified... Good enough for now.
let getProfessionalSyndic (connectionString: string) (organizationId: Guid) = async {
    let! result =
        Sql.connect connectionString
        |> Sql.query
            """
                SELECT
                    ps.OrganizationId,
                    psbuilding.BuildingId
                FROM
                    ProfessionalSyndics ps
                LEFT JOIN ProfessionalSyndicBuildings psBuilding on ps.OrganizationId = psBuilding.OrganizationId
                WHERE
                    ps.OrganizationId = @OrganizationId
            """
        |> Sql.parameters [ "@OrganizationId", Sql.uuid organizationId ]
        |> Sql.read readProfessionalSyndic

    match result with
    | [] ->
        return None
    | professionalSyndics ->
        match! Server.Organizations.Query.getOrganization connectionString organizationId with
        | Some organization ->
            return Some {
                Organization = organization
                BuildingIds = professionalSyndics |> List.choose (fun proSyndic -> proSyndic.BuildingId)
            }
        | None -> 
            return None
}

let getProfessionalSyndics connectionString () =
    Sql.connect connectionString
    |> Sql.query
        """
            SELECT
                pro.OrganizationId,
                org.Name,
                org.Address,
                org.MainEmailAddress,
                org.MainTelephoneNumber
            FROM ProfessionalSyndics pro
            LEFT JOIN Organizations org on pro.OrganizationId = org.OrganizationId
            WHERE pro.IsActive = TRUE
        """
    |> Sql.parameters []
    |> Sql.read readProfessionalSyndics

let getProfessionalSyndicsByIds connectionString (organizationIds: Guid list) =
    Sql.connect connectionString
    |> Sql.query
        """
            SELECT
                pro.OrganizationId,
                org.Name,
                org.Address,
                org.MainEmailAddress,
                org.MainTelephoneNumber
            FROM ProfessionalSyndics pro
            LEFT JOIN Organizations org on pro.OrganizationId = org.OrganizationId
            WHERE pro.IsActive = TRUE AND pro.OrganizationId = ANY (@OrganizationIds)
        """
    |> Sql.parameters [ "@OrganizationIds", Sql.uuidArray (organizationIds |> Seq.toArray) ]
    |> Sql.read readProfessionalSyndics