module Server.Lots.Query

open System
open Shared.Read
open Shared.Library
open Server.Library
open Npgsql.FSharp
open Server.PostgreSQL
open Server.PostgreSQL.Sql
open Server.Blueprint.Data.Financial

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
        BuildingId: Guid
        LotOwnerId: Guid
        PersonId: Guid option
        OrganizationId: Guid option
        StartDate: DateTime
        EndDate: DateTime option
    }

    let readLotOwner (reader: CaseInsensitiveRowReader): LotOwnerDbModel = {
        LotId = reader.uuid "LotId"
        BuildingId = reader.uuid "BuildingId"
        LotOwnerId = reader.uuid "LotOwnerId"
        OrganizationId = reader.uuidOrNone "OrganizationId"
        PersonId = reader.uuidOrNone "PersonId"
        StartDate = reader.dateTime "StartDate"
        EndDate = reader.dateTimeOrNone "EndDate"
    }

    let readLegalRepresentative (reader: CaseInsensitiveRowReader): LotOwnerTypeListItem option =
        let personId = reader.uuidOrNone "LegalRepresentativePersonId"
        let firstName = reader.stringOrNone "LegalRepresentativePersonFirstName"
        let lastName = reader.stringOrNone "LegalRepresentativePersonLastName"
        match personId, firstName, lastName with
        | Some pId, Some fName, Some lName -> Some (LotOwnerTypeListItem.Owner {| PersonId = pId; Name = sprintf "%s %s" fName lName |})
        | _ ->
            let orgId = reader.uuidOrNone "LegalRepresentativeOrgId"
            let orgName = reader.stringOrNone "LegalRepresentativeOrgName"
            match orgId, orgName with
            | Some orgId, Some orgName -> Some (LotOwnerTypeListItem.Organization {| OrganizationId = orgId; Name = orgName |})
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
                        lotOwner.BuildingId,
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
            BuildingId = dbRow.BuildingId
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

let private filterToQuery (filter: LotOwnerFilter) =
    match filter.Period with
    | LotOwnerFilterPeriod.FinancialYear financialYearId ->
        {|
            Join = "LEFT JOIN FinancialYears financialYear ON financialYear.FinancialYearId = lotOwner.FinancialYearId"
            Where = "lotOwner.FinancialYearId = @FinancialYearId AND lotOwner.StartDate <= financialYear.EndDate AND (lotOwner.EndDate IS NULL OR lotOwner.EndDate >= financialYear.StartDate)"
            Parameters = [ "@FinancialYearId", Sql.uuid financialYearId ]
        |}


let [<Literal>] private lotOwnersQuery =
    """
        SELECT lotOwner.LotId, lotOwner.BuildingId, lot.Code AS LotCode, lot.LotType, lotOwner.LotOwnerId, person.PersonId, person.FirstName AS PersonFirstName, person.LastName as PersonLastName, person.BankAccounts AS PersonBankAccounts, org.OrganizationId, org.Name AS OrganizationName, org.BankAccounts as OrganizationBankAccounts
        FROM LotOwners lotOwner
        LEFT JOIN Lots lot ON lotOwner.LotId = lot.LotId
        LEFT JOIN Persons person ON lotOwner.PersonId = person.PersonId
        LEFT JOIN Organizations organization ON lotOwner.OrganizationId = organization.OrganizationId
    """

let private readLotOwnerListItem (reader: CaseInsensitiveRowReader): FinancialLotOwner = {
    LotId = reader.uuid "LotId"
    BuildingId = reader.uuid "BuildingId"
    LotCode = reader.string "LotCode"
    LotType = LotType.OfString (reader.string "LotType")
    LotOwnerId = reader.uuid "LotOwnerId"
    LotOwnerType =
        match reader.uuidOrNone "PersonId", reader.uuidOrNone "OrganizationId" with
        | Some personId, _ -> 
            FinancialLotOwnerType.Owner 
                {| 
                    Name = [ reader.stringOrNone "PersonFirstName"; reader.stringOrNone "PersonLastName" ] |> List.choose id |> String.joinWith " "
                    PersonId = personId
                    BankAccounts = reader.stringOrNone "PersonBankAccounts" |> Option.either BankAccount.listFromJson [] 
                |}
        | _, Some orgId    -> 
            FinancialLotOwnerType.Organization 
                {| 
                    Name = reader.string "OrganizationName"
                    OrganizationId = orgId
                    BankAccounts = reader.stringOrNone "OrganizationBankAccounts" |> Option.either BankAccount.listFromJson [] 
                |}
        | _ -> failwithf "Failed to find the person / organization in the database"
}

let getLotOwnersByIds (conn: string) (identifiers: Guid list): Async<FinancialLotOwner list> =
    Sql.connect conn
    |> Sql.query
        (sprintf
            """
                %s
                WHERE lotOwner.LotOwnerId = ANY(@LotOwnerIds)
            """
            lotOwnersQuery)
    |> Sql.parameters [ "@LotOwnerIds", Sql.uuidArray (identifiers |> List.toArray) ]
    |> Sql.read readLotOwnerListItem

let getLotOwners (conn: string) (filter: LotOwnerFilter): Async<FinancialLotOwner list> =
    let query = filterToQuery filter
    Sql.connect conn
    |> Sql.query
        (sprintf
            """
                %s
                %s
                WHERE BuildingId = @BuildingId AND (%s)
            """
            lotOwnersQuery
            query.Join
            query.Where)
    |> Sql.parameters ([ "@BuildingId", Sql.uuid filter.BuildingId ] @ query.Parameters)
    |> Sql.read readLotOwnerListItem

/// Used by background services, this does a cross-boundry query, so don't use for any other purpose!
let getAllLotOwners (conn: string) (): Async<FinancialLotOwner list> =
    Sql.connect conn
    |> Sql.query lotOwnersQuery
    |> Sql.read readLotOwnerListItem