module Server.Lots.Query

open System
open Shared.Read
open Shared.Library
open Server.Library
open Npgsql.FSharp
open Server.PostgreSQL
open Server.PostgreSQL.Sql

[<AutoOpen>]
module private Readers =
    type LotDbModel = {
        LotId: Guid
        BuildingId: Guid
        Code: string
        LotType: string
        Description: string option
        Floor: int option
        Share: int option
    }

    let readLot (reader: CaseInsensitiveRowReader): LotDbModel = {
        LotId = reader.uuid "LotId"
        BuildingId = reader.uuid "BuildingId"
        Code = reader.string "Code"
        LotType = reader.string "LotType"
        Description = reader.stringOrNone "Description"
        Floor = reader.intOrNone "Floor"
        Share = reader.intOrNone "Share"
    }

    type LotOwnerDbModel = {
        LotId: Guid
        PersonId: Guid option
        OrganizationId: Guid option
        Role: LotOwnerRole
        StartDate: DateTime
        EndDate: DateTime option
    }

    let readLotOwner (reader: CaseInsensitiveRowReader): LotOwnerDbModel = {
        LotId = reader.uuid "LotId"
        OrganizationId = reader.uuidOrNone "OrganizationId"
        PersonId = reader.uuidOrNone "PersonId"
        Role =
            match reader.string "Role" with
            | x when x = string LotOwnerRole.LegalRepresentative -> LotOwnerRole.LegalRepresentative
            | _                                                  -> LotOwnerRole.Other
        StartDate = reader.dateTime "StartDate"
        EndDate = reader.dateTimeOrNone "EndDate"
    }

    let readLegalRepresentative (reader: CaseInsensitiveRowReader): LotOwnerListItem option =
        let personId = reader.uuidOrNone "LegalRepresentativePersonId"
        let firstName = reader.stringOrNone "LegalRepresentativePersonFirstName"
        let lastName = reader.stringOrNone "LegalRepresentativePersonLastName"
        match personId, firstName, lastName with
        | Some pId, Some fName, Some lName -> Some (LotOwnerListItem.Owner {| PersonId = pId; Name = sprintf "%s %s" fName lName |})
        | _ ->
            let orgId = reader.uuidOrNone "LegalRepresentativeOrgId"
            let orgName = reader.stringOrNone "LegalRepresentativeOrgName"
            match orgId, orgName with
            | Some orgId, Some orgName -> Some (LotOwnerListItem.Organization {| OrganizationId = orgId; Name = orgName |})
            | _ -> None

    let readLotListItem (reader: CaseInsensitiveRowReader): LotListItem = {
        LotId = reader.uuid "LotId"
        BuildingId = reader.uuid "BuildingId"
        Code = reader.string "Code"
        LotType = LotType.OfString (reader.string "LotType")
        Description = reader.stringOrNone "Description"
        Floor = reader.intOrNone "Floor"
        LegalRepresentative = readLegalRepresentative reader
        Share = reader.intOrNone "Share"
    }

let getLot (connectionString: string) (lotId: Guid): Async<Lot option> = async {
    let! lotDbModel =
        Sql.connect connectionString
        |> Sql.query
            """
                SELECT
                    LotId,
                    BuildingId,
                    Code,
                    LotType,
                    Description,
                    Floor,
                    Share
                FROM Lots
                WHERE LotId = @LotId
            """
        |> Sql.parameters [ "@LotId", Sql.uuid lotId ]
        |> Sql.readSingle readLot

    match lotDbModel with
    | Some dbModel ->
        let! lotOwnerDbRows =
            Sql.connect connectionString
            |> Sql.query "SELECT lotOwner.LotId, lotOwner.OrganizationId, lotOwner.PersonId, lotOwner.Role, lotOwner.StartDate, lotOwner.EndDate FROM LotOwners lotOwner WHERE lotId = @LotId"
            |> Sql.parameters [ "@LotId", Sql.uuid lotId ]
            |> Sql.read readLotOwner

        let ownerDbRowDictionary, orgDbRowDictionary = 
            lotOwnerDbRows
            |> List.partition (fun dbRow -> dbRow.PersonId.IsSome)
            |> (fun (ownerDbRows, orgDbRows) -> 
                ownerDbRows |> List.map (fun ownerDbRow -> (ownerDbRow.PersonId.Value, ownerDbRow)) |> dict,
                orgDbRows    |> List.map (fun orgDbRow -> (orgDbRow.OrganizationId.Value, orgDbRow)) |> dict
            )

        let mapLotOwnerDbRowAndRoleToLotOwner (lotOwnerType: LotOwnerType, dbRow: LotOwnerDbModel) = { 
            LotId = dbRow.LotId
            LotOwnerType = lotOwnerType
            LotOwnerRole = dbRow.Role
            StartDate = new DateTimeOffset(dbRow.StartDate.Year, dbRow.StartDate.Month, dbRow.StartDate.Day, 2, 0, 0, TimeSpan.FromHours(2.0))
            EndDate = dbRow.EndDate |> Option.map (fun endDate -> new DateTimeOffset(endDate.Year, endDate.Month, endDate.Day, 2, 0, 0, TimeSpan.FromHours(2.0)))
        }

        let! lotOwnersWithOwnerType =
            ownerDbRowDictionary.Keys
            |> List.ofSeq
            |> Server.Owners.Query.getOwnersByIds connectionString
            |> Async.map (List.map (fun li -> mapLotOwnerDbRowAndRoleToLotOwner (LotOwnerType.Owner li, ownerDbRowDictionary.[li.PersonId])))

        let! lotOwnersWithOrganizationType =
            orgDbRowDictionary.Keys
            |> List.ofSeq
            |> Server.Organizations.Query.getOrganizationsByIds connectionString
            |> Async.map (List.map (fun li -> mapLotOwnerDbRowAndRoleToLotOwner (LotOwnerType.Organization li, orgDbRowDictionary.[li.OrganizationId])))

        return Some {
            LotId = dbModel.LotId            
            BuildingId = dbModel.BuildingId
            Code = dbModel.Code
            LotType = LotType.OfString dbModel.LotType
            Description = dbModel.Description
            Floor = dbModel.Floor
            Owners = lotOwnersWithOrganizationType @ lotOwnersWithOwnerType
            Share = dbModel.Share
        }
    | None ->
        return None
}



let getLots (connectionString: string) (buildingId: Guid): Async<LotListItem list> =
    Sql.connect connectionString
    |> Sql.query
        (sprintf 
            """
                SELECT
                    lot.LotId,
                    lot.BuildingId,
                    person.PersonId as LegalRepresentativePersonId,
                    person.FirstName as LegalRepresentativePersonFirstName,
                    person.LastName as LegalRepresentativePersonLastName,
                    org.OrganizationId as LegalRepresentativeOrgId,
                    org.Name as LegalRepresentativeOrgName,
                    lot.Code,
                    lot.LotType,
                    lot.Description,
                    lot.Floor,
                    lot.Share
                FROM Lots lot
                LEFT JOIN LotOwners lotOwner on lot.LotId = lotOwner.LotId
                LEFT JOIN Persons person on lotOwner.PersonId = person.PersonId
                LEFT JOIN Organizations org on lotOwner.OrganizationId = org.OrganizationId
                WHERE lot.BuildingId = @BuildingId AND lot.IsActive = TRUE AND lotOwner.Role = '%s'
            """
            (string LotOwnerRole.LegalRepresentative)
        )
    |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
    |> Sql.read readLotListItem