module Server.Lots.Query

open System
open Shared.Read
open Server.Library
open Npgsql.FSharp
open Server.PostgreSQL
open Server.PostgreSQL.Sql

[<AutoOpen>]
module private Readers =
    type LotDbModel = {
        LotId: Guid
        BuildingId: Guid
        CurrentOwnerPersonId: Guid option
        CurrentOwnerOrganizationId: Guid option
        Code: string
        LotType: string
        Description: string option
        Floor: int option
        Surface: int option
    }

    let readLot (reader: CaseInsensitiveRowReader): LotDbModel = {
        LotId = reader.uuid "LotId"
        BuildingId = reader.uuid "BuildingId"
        CurrentOwnerPersonId = reader.uuidOrNone "CurrentOwnerPersonId"
        CurrentOwnerOrganizationId = reader.uuidOrNone "CurrentOwnerOrganizationId"
        Code = reader.string "Code"
        LotType = reader.string "LotType"
        Description = reader.stringOrNone "Description"
        Floor = reader.intOrNone "Floor"
        Surface = reader.intOrNone "Surface"
    }

    type LotListItemDbModel = {
        LotId: Guid
        BuildingId: Guid
        CurrentOwnerPersonId: Guid option
        CurrentOwnerPersonFirstName: string option
        CurrentOwnerPersonLastName: string option
        CurrentOwnerOrganizationId: Guid option
        CurrentOwnerOrganizationName: string option
        Code: string
        LotType: string
        Floor: int option
        Description: string option
    }

    let readLotListItem (reader: CaseInsensitiveRowReader): LotListItemDbModel = {
        LotId = reader.uuid "LotId"
        BuildingId = reader.uuid "BuildingId"
        CurrentOwnerPersonId = reader.uuidOrNone "CurrentOwnerPersonId"
        CurrentOwnerPersonFirstName = reader.stringOrNone "CurrentOwnerPersonFirstName"
        CurrentOwnerPersonLastName = reader.stringOrNone "CurrentOwnerPersonLastName"
        CurrentOwnerOrganizationId = reader.uuidOrNone "CurrentOwnerOrganizationId"
        CurrentOwnerOrganizationName = reader.stringOrNone "CurrentOwnerOrganizationName"
        Code = reader.string "Code"
        LotType = reader.string "LotType"
        Description = reader.stringOrNone "Description"
        Floor = reader.intOrNone "Floor"
    }

let getLot (connectionString: string) (lotId: Guid): Async<Lot option> = async {
    let! result =
        Sql.connect connectionString
        |> Sql.query
            """
                SELECT
                    LotId,
                    BuildingId,
                    CurrentOwnerPersonId,
                    CurrentOwnerOrganizationId,
                    Code,
                    LotType,
                    Description,
                    Floor,
                    Surface
                FROM Lots
                WHERE LotId = @LotId
            """
        |> Sql.parameters [ "@LotId", Sql.uuid lotId ]
        |> Sql.readSingle readLot

    match result with
    | Some dbModel ->
        let! owner =
            match dbModel.CurrentOwnerPersonId, dbModel.CurrentOwnerOrganizationId with
            | Some personId, _ ->
                Server.Owners.Query.getOwner connectionString personId
                |> Async.map (Option.map LotOwner.Owner)
            | _, Some organizationId -> 
                Server.Organizations.Query.getOrganization connectionString organizationId
                |> Async.map (Option.map LotOwner.Organization)
            | _ ->
                Async.lift None

        return Some {
            LotId = dbModel.LotId
            BuildingId = dbModel.BuildingId
            CurrentOwner = owner
            Code = dbModel.Code
            LotType = LotType.OfString dbModel.LotType
            Description = dbModel.Description
            Floor = dbModel.Floor
            Surface = dbModel.Surface
        }
    | None ->
        return None
}



let getLots (connectionString: string) (filter: {| BuildingId: Guid |}): Async<LotListItem list> = async {
    let! results =
        Sql.connect connectionString
        |> Sql.query
            """
                SELECT
                    lot.LotId,
                    lot.BuildingId,
                    lot.CurrentOwnerPersonId,
                    person.FirstName as CurrentOwnerPersonFirstName,
                    person.LastName as CurrentOwnerPersonLastName,
                    lot.CurrentOwnerOrganizationId,
                    org.Name as CurrentOwnerOrganizationName,
                    lot.Code,
                    lot.LotType,
                    lot.Description,
                    lot.Floor
                FROM Lots lot
                LEFT JOIN Persons person on lot.CurrentOwnerPersonId = person.PersonId
                LEFT JOIN Organizations org on lot.CurrentOwnerOrganizationId = org.OrganizationId
                WHERE lot.BuildingId = @BuildingId AND lot.IsActive = TRUE
            """
        |> Sql.parameters [ "@BuildingId", Sql.uuid filter.BuildingId ]
        |> Sql.read readLotListItem

    let mapOwner (dbModel: LotListItemDbModel): LotOwnerListItem option = 
        let forceString (str: string option) = defaultArg str ""
        match dbModel.CurrentOwnerPersonId, dbModel.CurrentOwnerOrganizationId with
        | Some personId, _ ->
            LotOwnerListItem.Person 
                {| 
                    PersonId = personId
                    Name = sprintf "%s %s" (dbModel.CurrentOwnerPersonFirstName |> forceString) (dbModel.CurrentOwnerPersonLastName |> forceString)
                |}
            |> Some
        | _, Some orgId ->
            LotOwnerListItem.Organization
                {|
                    OrganizationId = orgId
                    Name = dbModel.CurrentOwnerOrganizationName |> forceString
                |}
            |> Some
        | _ ->
            None

    return results |> List.map (fun dbModel -> {
        LotId = dbModel.LotId
        BuildingId = dbModel.BuildingId
        CurrentOwner = mapOwner dbModel
        Code = dbModel.Code
        LotType = LotType.OfString dbModel.LotType
        Description = dbModel.Description
        Floor = dbModel.Floor
    })
}