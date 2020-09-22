module Server.Financial.Query

open System
open Server.Library
open Shared.Read
open Npgsql.FSharp
open Server.PostgreSQL
open Server.PostgreSQL.Sql
open Shared.Write

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

let toInvoiceListItem (invoiceNumber: int, validated: ValidatedInvoice): InvoiceListItem = {
    InvoiceId = validated.InvoiceId
    BuildingId = validated.BuildingId
    FinancialYearCode = validated.FinancialYear.Code
    FinancialYearIsClosed = validated.FinancialYear.IsClosed
    InvoiceNumber = invoiceNumber
    Cost = validated.Cost
    VatRate = validated.VatRate.Value ()
    DistributionKeyName = validated.DistributionKey.Name
    OrganizationName = validated.OrganizationName
    CategoryCode = string validated.CategoryCode
    DueDate = validated.DueDate
    HasBeenPaid = not validated.PaymentIds.IsEmpty
}

let toInvoice (invoice: ValidatedInvoice): Invoice = {
    InvoiceId = invoice.InvoiceId
    BuildingId = invoice.BuildingId
    FinancialYear = invoice.FinancialYear
    InvoiceNumber = 0
    Description = invoice.Description
    Cost = invoice.Cost
    VatRate = invoice.VatRate.Value ()
    CategoryCode = string invoice.CategoryCode
    CategoryDescription = string invoice.CategoryDescription
    FromBankAccountType = string invoice.FromBankAccountType
    FromBankAccountIBAN = string invoice.FromBankAccountIBAN
    FromBankAccountBIC = string invoice.FromBankAccountBIC
    ToBankAccountIBAN = string invoice.ToBankAccountIBAN
    ToBankAccountBIC = string invoice.ToBankAccountBIC
    BookingDate = invoice.BookingDate
    DistributionKey = invoice.DistributionKey
    OrganizationId = invoice.OrganizationId
    OrganizationName = invoice.OrganizationName
    OrganizationNumber = invoice.OrganizationNumber
    OrganizationVatNumber = invoice.OrganizationVatNumber
    ExternalInvoiceNumber = invoice.ExternalInvoiceNumber
    InvoiceDate = invoice.InvoiceDate
    DueDate = invoice.DueDate
    PaymentIds = invoice.PaymentIds
}

let getInvoices (conn: string) (filter: InvoiceFilter) =
    Server.Financial.Storage.invoices 
    |> List.filter (fun invoice -> invoice.BuildingId = filter.BuildingId)
    |> List.mapi (fun index invoice -> toInvoiceListItem (index, invoice))
    |> Async.lift

let getInvoice (conn: string) (invoiceId: Guid) =
    Server.Financial.Storage.invoices
    |> List.tryFind (fun invoice -> invoice.InvoiceId = invoiceId)
    |> Option.map (fun invoice -> toInvoice invoice)
    |> Async.lift

let dummyFinancialYears: FinancialYear list = [
    {
        FinancialYearId = Guid.NewGuid()
        Code = "2020/Q1"
        StartDate = DateTimeOffset.Now
        EndDate = DateTimeOffset.Now.AddDays(5.0)
        IsClosed = true
    }
    {
        FinancialYearId = Guid.NewGuid()
        Code = "2020/Q2"
        StartDate = DateTimeOffset.Now
        EndDate = DateTimeOffset.Now.AddDays(5.0)
        IsClosed = true
    }
    {
        FinancialYearId = Guid.NewGuid()
        Code = "2020/Q3"
        StartDate = DateTimeOffset.Now
        EndDate = DateTimeOffset.Now.AddDays(5.0)
        IsClosed = true
    }
]
let getFinancialYears (conn: string) (buildingId: BuildingId) =
    dummyFinancialYears
    |> Async.lift

let dummyFinancialCategories: FinancialCategory list = [
    {
        FinancialCategoryId = Guid.NewGuid()
        Code = "45"
        Description = "Algemene kosten"
    }
    {
        FinancialCategoryId = Guid.NewGuid()
        Code = "450"
        Description = "Waterkosten"
    }
    {
        FinancialCategoryId = Guid.NewGuid()
        Code = "451"
        Description = "Gaskosten"
    }
    {
        FinancialCategoryId = Guid.NewGuid()
        Code = "452"
        Description = "Electriciteitskosten"
    }
]
let getFinancialCategories (conn: string) (buildingId: BuildingId) =
    dummyFinancialCategories
    |> Async.lift