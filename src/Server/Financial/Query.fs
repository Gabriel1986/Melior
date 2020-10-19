module Server.Financial.Query

open System
open Server.Library
open Shared.Read
open Npgsql.FSharp
open Server.PostgreSQL
open Server.PostgreSQL.Sql
open Shared.MediaLibrary

type DistributionKeyRow = {
    DistributionKeyId: Guid
    BuildingId: BuildingId option
    Name: string
    DistributionType: DistributionType
    LotId: Guid option
    LotType: LotType option
}

let mapToLotsOrLotTypes (rows: DistributionKeyRow list): LotsOrLotTypes =
    let first = rows |> List.head
    match first.LotId, first.LotType with
    | None, None -> LotsOrLotTypes.Lots []
    | Some _, _  -> LotsOrLotTypes.Lots (rows |> List.choose (fun r -> r.LotId))
    | _, Some _  -> LotsOrLotTypes.LotTypes (rows |> List.choose (fun r -> r.LotType))

let readDistributionType (reader: CaseInsensitiveRowReader) (columnName: string): DistributionType =
    match reader.string columnName with
    | x when x = string DistributionType.EqualParts -> 
        DistributionType.EqualParts
    | _ -> 
        DistributionType.Shares

let readDistributionKey (reader: CaseInsensitiveRowReader): DistributionKeyRow = {
    DistributionKeyId = reader.uuid "DistributionKeyId"
    BuildingId = reader.uuidOrNone "BuildingId"
    Name = reader.string "Name"
    DistributionType = readDistributionType reader "DistributionType"
    LotId = reader.uuidOrNone "LotId"
    LotType = (reader.stringOrNone "LotType" |> Option.map LotType.OfString)
}

let readDistributionKeyListItem (reader: CaseInsensitiveRowReader): DistributionKeyListItem = {
    DistributionKeyId = reader.uuid "DistributionKeyId"
    BuildingId = reader.uuidOrNone "BuildingId"
    Name = reader.string "Name"
    DistributionType = readDistributionType reader "DistributionType"
}

type InvoiceDbRow = {
    InvoiceId: Guid
    BuildingId: Guid
    FinancialYearId: Guid
    InvoiceNumber: int
    Description: string option
    Cost: Decimal
    VatRate: int
    FinancialCategoryId: Guid
    BookingDate: DateTime //Date when booked
    DistributionKeyId: Guid
    OrganizationId: Guid
    OrganizationBankAccount: BankAccount
    OrganizationInvoiceNumber: string option //Number @ supplier
    InvoiceDate: DateTime //Date on the invoice
    DueDate: DateTime option //Due date of the invoice
}

let readInvoice (reader: CaseInsensitiveRowReader): InvoiceDbRow = {
    InvoiceId = reader.uuid "InvoiceId"
    BuildingId = reader.uuid "BuildingId"
    FinancialYearId = reader.uuid "FinancialYearId"
    InvoiceNumber = reader.int "InvoiceNumber"
    Description = reader.stringOrNone "Description"
    Cost = reader.decimal "Cost"
    VatRate = reader.int "VatRate"
    FinancialCategoryId = reader.uuid "FinancialCategoryId"
    BookingDate = reader.dateTime "BookingDate"
    DistributionKeyId = reader.uuid "DistributionKeyId"
    OrganizationId = reader.uuid "OrganizationId"
    OrganizationBankAccount = (reader.string "OrganizationBankAccount") |> BankAccount.fromJson
    OrganizationInvoiceNumber = reader.stringOrNone "OrganizationInvoiceNumber"
    InvoiceDate = reader.dateTime "InvoiceDate"
    DueDate = reader.dateTimeOrNone "DueDate"
}

let readInvoiceListItem (reader: CaseInsensitiveRowReader): InvoiceListItem = {
    InvoiceId = reader.uuid "InvoiceId"
    BuildingId = reader.uuid "BuildingId"
    FinancialYearCode = reader.string "FinancialYearCode"
    FinancialYearIsClosed = reader.bool "FinancialYearIsClosed"
    InvoiceNumber = reader.int "InvoiceNumber"
    Cost = reader.decimal "Cost"
    DistributionKeyName = reader.string "DistributionKeyName"
    OrganizationName = reader.string "OrganizationName"
    CategoryCode = reader.string "CategoryCode"
    CategoryDescription = reader.string "CategoryDescription"
    InvoiceDate = reader.dateTime "InvoiceDate"
    DueDate = reader.dateTimeOrNone "DueDate"
}

let readFinancialYear (reader: CaseInsensitiveRowReader): FinancialYear = {
    FinancialYearId = reader.uuid "FinancialYearId"
    BuildingId = reader.uuid "BuildingId"
    Code = reader.string "Code"
    StartDate = reader.dateTimeOrNone "StartDate"
    EndDate = reader.dateTimeOrNone "EndDate"
    IsClosed = reader.bool "IsClosed"
}

let readFinancialCategory (reader: CaseInsensitiveRowReader): FinancialCategory = {
    FinancialCategoryId = reader.uuid "FinancialCategoryId"
    BuildingId = reader.uuid "BuildingId"
    Code = reader.string "Code"
    Description = reader.string "Description"
}

let getDistributionKeyListItems (conn: string) (buildingId: BuildingId): Async<DistributionKeyListItem list> =
    Sql.connect conn
    |> Sql.query
        """
            SELECT
                dKey.DistributionKeyId,
                dKey.BuildingId,
                dKey.Name,
                dKey.DistributionType
            FROM DistributionKeys dKey
            WHERE (dKey.BuildingId = @BuildingId OR dKey.BuildingId IS NULL) AND dKey.IsActive = TRUE
        """
    |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
    |> Sql.read readDistributionKeyListItem

let getDistributionKeys (conn: string) (buildingId: BuildingId): Async<DistributionKey list> = async {
    let! distributionKeyRows =
        Sql.connect conn
        |> Sql.query
            """
                SELECT
                    dKey.DistributionKeyId,
                    dKey.BuildingId,
                    dKey.Name,
                    dKey.DistributionType
                    link.LotId, 
                    link.LotType
                FROM DistributionKeys dKey
                LEFT JOIN DistributionKeyLotsOrLotTypes links WHERE link.DistributionKeyId = @DistributionKeyId
                WHERE (dKey.BuildingId = @BuildingId OR dKey.BuildingId IS NULL) AND dKey.IsActive = TRUE
            """
        |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
        |> Sql.read readDistributionKey

    return
        distributionKeyRows
        |> List.groupBy (fun row -> row.DistributionKeyId)
        |> List.map (fun (_, distributionKeys) ->
            let first = distributionKeys |> List.head
            {
                DistributionKeyId = first.DistributionKeyId
                BuildingId = first.BuildingId
                Name = first.Name
                DistributionType = first.DistributionType
                LotsOrLotTypes = distributionKeys |> mapToLotsOrLotTypes
            }    
        )
}

let getDistributionKeysByIds (conn: string) (buildingId: BuildingId, distributionKeyIds: Guid list): Async<DistributionKeyListItem list> =
    Sql.connect conn
    |> Sql.query
        """
            SELECT
                DistributionKeyId,
                BuildingId,
                Name,
                DistributionKeyType
            FROM DistributionKeys WHERE BuildingId = @BuildingId AND DistributionKeyId = ANY (@DistributionKeyIds) AND IsDeleted = FALSE
        """
    |> Sql.parameters [ 
        "@DistributionKeyIds", Sql.uuidArray (distributionKeyIds |> List.toArray)
        "@BuildingId", Sql.uuid buildingId
    ]
    |> Sql.read readDistributionKeyListItem

let getInvoices (conn: string) (filter: InvoiceFilter): Async<InvoiceListItem list> =
    let whereFilter, whereParameters =
        match filter.Period with
        | InvoiceFilterPeriod.FinancialYear code -> 
            "FinancialYearCode = @FinancialYearCode", [ "@FinancialYearCode", Sql.string code ]
        | InvoiceFilterPeriod.Month (month, year) ->
            let date = new DateTime(year, month, 1, 0, 0, 0)
            "InvoiceDate >= @FilterStartDate AND InvoiceDate < @FilterEndDate", [
                "@FilterStartDate", Sql.timestamp date 
                "@FilterEndDate", Sql.timestamp (date.AddMonths(1))
            ]
        | InvoiceFilterPeriod.Year year ->
            let date = new DateTime(year, 1, 1)
            "InvoiceDate >= @FilterStartDate AND InvoiceDate < @FilterEndDate", [
                "@FilterStartDate", Sql.timestamp date
                "@FilterEndDate", Sql.timestamp (date.AddYears(1))
            ]

    Sql.connect conn
    |> Sql.query
        (sprintf
            """
                SELECT
                    invoice.InvoiceId,
                    invoice.BuildingId,
                    (SELECT IsClosed AS FinancialYearIsClosed, Code AS FinancialYearCode FROM FinancialYears fYear WHERE fYear.Code = invoice.FinancialYearCode) AS FinancialYearIsClosed,
                    invoice.InvoiceNumber,
                    invoice.Cost,
                    (SELECT Name AS DistributionKeyName FROM DistributionKeys dKey WHERE dKey.DistributionKeyId = invoice.DistributionKeyId),
                    invoice.OrganizationName,
                    (SELECT Code AS CategoryCode, Description AS CategoryDescription FROM FinancialCategories cat WHERE cat.FinancialCategoryId = invoice.FinancialCategoryId),
                    invoice.InvoiceDate,
                    invoice.DueDate
                FROM Invoices
                WHERE BuildingId = @BuildingId AND %s
            """
            whereFilter)
    |> Sql.parameters ([ "@BuildingId", Sql.uuid filter.BuildingId ] @ whereParameters)
    |> Sql.read readInvoiceListItem

let getAllFinancialYears (conn: string) (buildingId: BuildingId): Async<FinancialYear list> =
    Sql.connect conn
    |> Sql.query 
        """
            SELECT
                FinancialYearId,
                BuildingId,
                Code,
                StartDate,
                EndDate,
                IsClosed
            FROM FinancialYears WHERE BuildingId = @BuildingId AND IsDeleted = FALSE
        """
    |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
    |> Sql.read readFinancialYear


let getFinancialYearsByIds (conn: string) (buildingId: BuildingId, financialYearIds: Guid list): Async<FinancialYear list> =
    Sql.connect conn
    |> Sql.query 
        """
            SELECT
                FinancialYearId,
                BuildingId,
                Code,
                StartDate,
                EndDate,
                IsClosed
            FROM FinancialYears WHERE BuildingId = @BuildingId AND FincialYearId = ANY (@FinancialYearIds) AND IsDeleted = FALSE
        """
    |> Sql.parameters [
        "@BuildingId", Sql.uuid buildingId
        "@FinancialYearIds", Sql.uuidArray (financialYearIds |> List.toArray)
    ]
    |> Sql.read readFinancialYear

let getAllFinancialCategories (conn: string) (buildingId: BuildingId) =
    Sql.connect conn
    |> Sql.query 
        """
            SELECT
                FinancialCategoryId,
                ParentFinancialCategoryId,
                BuildingId,
                Code,
                Description
            FROM FinancialCategories WHERE BuildingId = @BuildingId AND IsDeleted = FALSE
        """
    |> Sql.parameters [
        "@BuildingId", Sql.uuid buildingId
    ]
    |> Sql.read readFinancialCategory

let getFinancialCategoriesByIds (conn: string) (buildingId: BuildingId, financialCategoryIds: Guid list): Async<FinancialCategory list> =
    Sql.connect conn
    |> Sql.query 
        """
            SELECT
                FinancialCategoryId,
                BuildingId,
                Code,
                Description
            FROM FinancialCategories WHERE BuildingId = @BuildingId AND FincialCategoryId = ANY (@FinancialCategoryIds) AND IsDeleted = FALSE
        """
    |> Sql.parameters [
        "@BuildingId", Sql.uuid buildingId
        "@FinancialCategoryIds", Sql.uuidArray (financialCategoryIds |> List.toArray)
    ]
    |> Sql.read readFinancialCategory

let getInvoice (conn: string) (invoiceId: Guid): Async<Invoice option> = async {
    let! dbRowOption =
        Sql.connect conn
        |> Sql.query
            """
                SELECT
                    InvoiceId,
                    BuildingId,
                    FinancialYearId,
                    InvoiceNumber,
                    Description,
                    Cost,
                    VatRate,
                    FinancialCategoryId,
                    BookingDate,
                    DistributionKeyId,
                    OrganizationId,
                    OrganizationBankAccount,
                    OrganizationInvoiceNumber,
                    InvoiceDate,
                    DueDate
                FROM Invoices
                WHERE InvoiceId = @InvoiceId
            """
        |> Sql.parameters [ "@InvoiceId", Sql.uuid invoiceId ]
        |> Sql.readSingle readInvoice

    match dbRowOption with
    | Some dbRow ->
        let! financialYear = getFinancialYearsByIds conn (dbRow.BuildingId, [ dbRow.FinancialYearId ]) |> Async.map List.head
        let! financialCategory = getFinancialCategoriesByIds conn (dbRow.BuildingId, [ dbRow.FinancialCategoryId ]) |> Async.map List.head
        let! distributionKey = getDistributionKeysByIds conn (dbRow.BuildingId, [ dbRow.DistributionKeyId ]) |> Async.map List.head
        let! mediaFiles = Server.Media.Query.getMediaFilesForEntities conn (Partitions.Invoices) [ dbRow.InvoiceId ]
        let! organization = Server.Organizations.Query.getOrganizationsByIds conn [ dbRow.OrganizationId ] |> Async.map List.head
        return Some {
            InvoiceId = dbRow.InvoiceId
            BuildingId = dbRow.BuildingId
            FinancialYear = financialYear
            InvoiceNumber = dbRow.InvoiceNumber
            Description = dbRow.Description
            Cost = dbRow.Cost
            VatRate = dbRow.VatRate
            FinancialCategory = financialCategory
            BookingDate = dbRow.BookingDate
            DistributionKey = distributionKey
            Organization = organization
            OrganizationBankAccount = dbRow.OrganizationBankAccount
            OrganizationInvoiceNumber = dbRow.OrganizationInvoiceNumber
            InvoiceDate = dbRow.InvoiceDate
            DueDate = dbRow.DueDate
            MediaFiles = mediaFiles
        }
    | None ->
        return None
}