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
    }

    let readProfessionalSyndic (reader: CaseInsensitiveRowReader): ProfessionalSyndicDbModel = {
        OrganizationId = reader.uuid "PersonId"
    }

    let readProfessionalSyndics (reader: CaseInsensitiveRowReader): ProfessionalSyndicListItem = {
        OrganizationId = reader.uuid "OrganizationId"
        Name = reader.string "Name"
        Address = 
            let addrString = reader.stringOrNone "Address"
            match addrString with
            | Some str -> Address.fromJson str |> Option.fromResult |> Option.defaultValue Address.Init
            | None -> Address.Init
        MainEmailAddress = reader.stringOrNone "MainEmailAddress"
        MainTelephoneNumber = reader.stringOrNone "MainTelephoneNumber"
    }

//Can probably be simplified...
let getProfessionalSyndic (connectionString: string) (organizationId: Guid) = async {
    let! result =
        Sql.connect connectionString
        |> Sql.query
            """
                SELECT
                    OrganizationId
                FROM
                    ProfessionalSyndics
                WHERE
                    OrganizationId = @OrganizationId
            """
        |> Sql.parameters [ "@OrganizationId", Sql.uuid organizationId ]
        |> Sql.readSingle readProfessionalSyndic

    match result with
    | Some dbModel ->
        match! Server.Organizations.Query.getOrganization connectionString dbModel.OrganizationId with
        | Some organization ->
            return Some {
                Organization = organization
            }
        | None -> 
            return None
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
        """
    |> Sql.parameters []
    |> Sql.read readProfessionalSyndics