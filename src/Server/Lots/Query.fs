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
        LotOwnerId: Guid
        PersonId: Guid option
        OrganizationId: Guid option
        StartDate: DateTime
        EndDate: DateTime option
    }

    let readLotOwner (reader: CaseInsensitiveRowReader): LotOwnerDbModel = {
        LotId = reader.uuid "LotId"
        LotOwnerId = reader.uuid "LotOwnerId"
        OrganizationId = reader.uuidOrNone "OrganizationId"
        PersonId = reader.uuidOrNone "PersonId"
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

let getLotOwnerContactsByLotOwnerId (connectionString: string) (lotOwnerId: Guid): Async<LotOwnerContact list> =
    Sql.connect connectionString
    |> Sql.query
        """
            SELECT BuildingId, ContactOwnerId, ContactPersonId FROM LotOwnerContacts WHERE LotOwnerId = @LotOwnerId
        """
    |> Sql.parameters [
        "@LotOwnerId", Sql.uuid lotOwnerId
    ]
    |> Sql.read (fun reader -> 
        {| 
            BuildingId = reader.uuidOrNone "BuildingId"
            PersonId = reader.uuidOrNone "ContactPersonId"
            OwnerId = reader.uuidOrNone "ContactOwnerId"
        |})
    |> Async.bind (
        fun results -> async {
            let personResults, ownerResults = results |> List.partition (fun r -> r.PersonId.IsSome)
            let persons = 
                personResults 
                |> List.map (fun personResult -> personResult.PersonId.Value)
                |> Server.Persons.Query.getPersonsByIds connectionString
                |> Async.map (List.map LotOwnerContact.NonOwner)
            let owners =
                ownerResults
                |> List.map (fun ownerResult -> ownerResult.OwnerId.Value)
                |> Server.Owners.Query.getOwnersByIds connectionString
                |> Async.map (List.map LotOwnerContact.Owner)

            return!
                Async.Parallel [
                    persons
                    owners
                ]
                |> Async.map (Array.toList >> List.collect id)
        })

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
                WHERE LotId = @LotId AND IsActive = TRUE
            """
        |> Sql.parameters [ "@LotId", Sql.uuid lotId ]
        |> Sql.readSingle readLot

    match lotDbModel with
    | Some dbModel ->
        let! lotOwnerDbRows =
            Sql.connect connectionString
            |> Sql.query 
                """
                    SELECT
                        lotOwner.LotId,
                        lotOwner.LotOwnerId,
                        lotOwner.OrganizationId,
                        lotOwner.PersonId,
                        lotOwner.StartDate,
                        lotOwner.EndDate
                    FROM LotOwners lotOwner WHERE lotId = @LotId AND IsDeleted = FALSE
                """
            |> Sql.parameters [ "@LotId", Sql.uuid lotId ]
            |> Sql.read readLotOwner

        let ownerDbRowDictionary, orgDbRowDictionary = 
            lotOwnerDbRows
            |> List.partition (fun dbRow -> dbRow.PersonId.IsSome)
            |> (fun (ownerDbRows, orgDbRows) -> 
                ownerDbRows |> List.map (fun ownerDbRow -> (ownerDbRow.PersonId.Value, ownerDbRow)) |> dict,
                orgDbRows |> List.map (fun orgDbRow -> (orgDbRow.OrganizationId.Value, orgDbRow)) |> dict
            )

        let mapLotOwnerDbRowAndRoleToLotOwner (lotOwnerType: LotOwnerType, contacts: LotOwnerContact list, dbRow: LotOwnerDbModel) = { 
            LotId = dbRow.LotId
            LotOwnerId = dbRow.LotOwnerId
            LotOwnerType = lotOwnerType
            StartDate = new DateTimeOffset(dbRow.StartDate.Year, dbRow.StartDate.Month, dbRow.StartDate.Day, 2, 0, 0, TimeSpan.FromHours(2.0))
            EndDate = dbRow.EndDate |> Option.map (fun endDate -> new DateTimeOffset(endDate.Year, endDate.Month, endDate.Day, 2, 0, 0, TimeSpan.FromHours(2.0)))
            Contacts = contacts
        }

        let! lotOwnersWithOwnerType =
            ownerDbRowDictionary.Keys
            |> List.ofSeq
            |> Server.Owners.Query.getOwnersByIds connectionString
            |> Async.bind (
                List.map (fun (owner: OwnerListItem) -> async {
                    let lotOwner = ownerDbRowDictionary.[owner.PersonId]
                    let! contacts = getLotOwnerContactsByLotOwnerId connectionString lotOwner.LotOwnerId
                    return mapLotOwnerDbRowAndRoleToLotOwner (LotOwnerType.Owner owner, contacts, lotOwner)
                })
                >> Async.Parallel)

        let! lotOwnersWithOrganizationType =
            orgDbRowDictionary.Keys
            |> List.ofSeq
            |> Server.Organizations.Query.getOrganizationsByIds connectionString
            |> Async.bind (
                List.map (fun (organization: OrganizationListItem) -> async {
                    let lotOwner = orgDbRowDictionary.[organization.OrganizationId]
                    let! contacts = getLotOwnerContactsByLotOwnerId connectionString lotOwner.LotOwnerId
                    return mapLotOwnerDbRowAndRoleToLotOwner (LotOwnerType.Organization organization, contacts, lotOwner)
                })
                >> Async.Parallel)

        return Some {
            LotId = dbModel.LotId            
            BuildingId = dbModel.BuildingId
            Code = dbModel.Code
            LotType = LotType.OfString dbModel.LotType
            Description = dbModel.Description
            Floor = dbModel.Floor
            Owners = [ yield! lotOwnersWithOrganizationType; yield! lotOwnersWithOwnerType ]
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
                SELECT DISTINCT
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
                LEFT JOIN LotOwners lotOwner on lot.LotId = lotOwner.LotId AND (lotOwner IS NULL OR ((NOT lotOwner.IsDeleted) AND lotOwner.StartDate < @Now AND (lotOwner.EndDate IS NULL OR lotOwner.EndDate > @Now)))
                LEFT JOIN Persons person on lotOwner.PersonId = person.PersonId
                LEFT JOIN Organizations org on lotOwner.OrganizationId = org.OrganizationId
                WHERE lot.BuildingId = @BuildingId AND lot.IsActive = TRUE
            """
        )
    |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId; "@Now", Sql.timestamp DateTime.Now ]
    |> Sql.read readLotListItem