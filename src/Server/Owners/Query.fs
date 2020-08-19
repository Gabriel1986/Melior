module Server.Owners.Query

open System
open Npgsql.FSharp
open Server.Library
open Server.PostgreSQL
open Server.PostgreSQL.Sql
open Shared.Read

[<AutoOpen>]
module private Readers =
    type OwnerDbModel = {
        PersonId: Guid
        BuildingId: Guid
        IsResident: bool
    }

    let readOwner (reader: CaseInsensitiveRowReader): OwnerDbModel = {
        PersonId = reader.uuid "PersonId"
        BuildingId = reader.uuid "BuildingId"
        IsResident = reader.bool "IsResident"
    }

    let ownerListItemQuery =
        """
            SELECT
                owner.PersonId,
                owner.BuildingId,
                owner.IsResident,
                person.FirstName,
                person.LastName
            FROM
                Owners owner
            LEFT JOIN Persons person on person.PersonId = owner.PersonId 
            WHERE owner.IsActive = TRUE
        """

    let readOwners (reader: CaseInsensitiveRowReader): OwnerListItem = {
            PersonId = reader.uuid "PersonId"
            BuildingId = reader.uuid "BuildingId"
            FirstName = reader.stringOrNone "FirstName"
            LastName = reader.stringOrNone "LastName"
            IsResident = reader.bool "IsResident"
        }

let getOwner (connectionString: string) (ownerId: Guid) = async {
    let! result =
        Sql.connect connectionString
        |> Sql.query
            """
                SELECT
                    PersonId,
                    BuildingId,
                    IsResident
                FROM
                    Owners
                WHERE
                    PersonId = @PersonId
            """
        |> Sql.parameters [ "@PersonId", Sql.uuid ownerId ]
        |> Sql.readSingle readOwner

    match result with
    | Some dbModel ->
        match! Server.Persons.Query.getPerson connectionString ownerId with
        | Some person ->
            return Some {
                Person = person
                BuildingId = dbModel.BuildingId
                IsResident = dbModel.IsResident
            }
        | None -> 
            return None
    | None ->
        return None
}

let getOwners (connectionString: string) (filter: {| BuildingId: Guid |}) =
    Sql.connect connectionString
    |> Sql.query (ownerListItemQuery + " AND BuildingId = @BuildingId")
    |> Sql.parameters [
        "@BuildingId", Sql.uuid filter.BuildingId
    ]
    |> Sql.read readOwners

let getOwnersByIds conn (personIds: Guid list) =
    match personIds with
    | [] -> 
        Async.lift []
    | personIds ->
        Sql.connect conn
        |> Sql.query (ownerListItemQuery + " AND owner.PersonId = ANY (@PersonIds)")
        |> Sql.parameters [ 
            "@PersonIds", Sql.uuidArray (personIds |> List.toArray) 
        ]
        |> Sql.read readOwners
