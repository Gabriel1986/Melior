module Server.Financial.Query

open System
open Server.Library
open Shared.Library
open Shared.Read
open Npgsql.FSharp
open Server.PostgreSQL
open Server.PostgreSQL.Sql
open Shared.MediaLibrary
open Server.Blueprint.Data.Financial
open Shared.Write

type DistributionKeyRow = {
    DistributionKeyId: Guid
    BuildingId: BuildingId option
    Name: string
    DistributionType: DistributionType
    LotId: Guid option
    LotType: LotType option
    IncludeGroundFloor: bool
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
    IncludeGroundFloor = reader.bool "IncludeGroundFloor"
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
    DueDate: DateTime //Due date of the invoice
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
    DueDate = reader.dateTime "DueDate"
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
    OrganizationId = reader.uuid "OrganizationId"
    FinancialCategoryCode = reader.string "CategoryCode"
    FinancialCategoryDescription = reader.string "CategoryDescription"
    InvoiceDate = 
        let dt = reader.dateTime "InvoiceDate"
        new DateTimeOffset(dt.Year, dt.Month, dt.Day, 2, 0, 0, TimeSpan.FromHours(2.0))
    DueDate = 
        let dt = reader.dateTime "DueDate"
        new DateTimeOffset(dt.Year, dt.Month, dt.Day, 2, 0, 0, TimeSpan.FromHours(2.0))
    IsPaid =
        let cost = reader.decimal "Cost"
        let paidAmount = reader.decimalOrNone "PaidAmount" |> Option.defaultValue Decimal.Zero
        cost - paidAmount = Decimal.Zero
}

let readFinancialYear (reader: CaseInsensitiveRowReader): FinancialYear = {
    FinancialYearId = reader.uuid "FinancialYearId"
    BuildingId = reader.uuid "BuildingId"
    Code = reader.string "Code"
    StartDate = reader.dateTime "StartDate"
    EndDate = reader.dateTime "EndDate"
    IsClosed = reader.bool "IsClosed"
}

let readFinancialCategory (reader: CaseInsensitiveRowReader): FinancialCategory = {
    FinancialCategoryId = reader.uuid "FinancialCategoryId"
    BuildingId = reader.uuid "BuildingId"
    Code = reader.string "Code"
    Description = reader.string "Description"
    LotOwnerId = reader.uuidOrNone "LotOwnerId"
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

let private convertToDistributionKeys =
    List.groupBy (fun (row: DistributionKeyRow) -> row.DistributionKeyId)
    >> List.map (fun (_, distributionKeys) ->
        let first = distributionKeys |> List.head
        {
            DistributionKeyId = first.DistributionKeyId
            BuildingId = first.BuildingId
            Name = first.Name
            DistributionType = first.DistributionType
            LotsOrLotTypes = distributionKeys |> mapToLotsOrLotTypes
            IncludeGroundFloor = first.IncludeGroundFloor
        }    
    )

let getDistributionKeys (conn: string) (buildingId: BuildingId): Async<DistributionKey list> = async {
    let! distributionKeyRows =
        Sql.connect conn
        |> Sql.query
            """
                SELECT
                    dKey.DistributionKeyId,
                    dKey.BuildingId,
                    dKey.Name,
                    dKey.DistributionType,
                    dKey.IncludeGroundFloor,
                    link.LotId, 
                    link.LotType
                FROM DistributionKeys dKey
                LEFT JOIN DistributionKeyLotsOrLotTypes link ON link.DistributionKeyId = dKey.DistributionKeyId
                WHERE (dKey.BuildingId = @BuildingId OR dKey.BuildingId IS NULL) AND dKey.IsActive = TRUE
            """
        |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
        |> Sql.read readDistributionKey

    return distributionKeyRows |> convertToDistributionKeys
}

let getDistributionKey (conn: string) (distributionKeyId: Guid) = async {
    let! distributionKeyRows =
        Sql.connect conn
        |> Sql.query            
            """
                SELECT
                    dKey.DistributionKeyId,
                    dKey.BuildingId,
                    dKey.Name,
                    dKey.DistributionType,
                    dKey.IncludeGroundFloor,
                    link.LotId, 
                    link.LotType
                FROM DistributionKeys dKey
                LEFT JOIN DistributionKeyLotsOrLotTypes link ON link.DistributionKeyId = dKey.DistributionKeyId
                WHERE dKey.DistributionKeyId = @DistributionKeyId AND dKey.IsActive = TRUE
            """
        |> Sql.parameters [ "@DistributionKeyId", Sql.uuid distributionKeyId ]
        |> Sql.read readDistributionKey

    return 
        distributionKeyRows 
        |> convertToDistributionKeys 
        |> List.tryHead
}

let getDistributionKeysByIds (conn: string) (buildingId: BuildingId, distributionKeyIds: Guid list): Async<DistributionKeyListItem list> =
    Sql.connect conn
    |> Sql.query
        """
            SELECT
                DistributionKeyId,
                BuildingId,
                Name,
                DistributionType
            FROM DistributionKeys WHERE (BuildingId = @BuildingId OR BuildingId IS NULL) AND DistributionKeyId = ANY (@DistributionKeyIds)
        """
    |> Sql.parameters [ 
        "@DistributionKeyIds", Sql.uuidArray (distributionKeyIds |> List.toArray)
        "@BuildingId", Sql.uuid buildingId
    ]
    |> Sql.read readDistributionKeyListItem

let getInvoices (conn: string) (filter: FinancialTransactionFilter): Async<InvoiceListItem list> =
    let whereFilter, whereParameters =
        match filter.Period with
        | FinancialTransactionFilterPeriod.FinancialYear financialYearId -> 
            "invoice.FinancialYearId = @FinancialYearId", [
                "@FinancialYearId", Sql.uuid financialYearId
            ]
        | FinancialTransactionFilterPeriod.Month (month, year) ->
            let date = new DateTime(year, month, 1)
            "invoice.InvoiceDate >= @FilterStartDate AND invoice.InvoiceDate < @FilterEndDate", [
                "@FilterStartDate", Sql.timestamp date 
                "@FilterEndDate", Sql.timestamp (date.AddMonths(1))
            ]
        | FinancialTransactionFilterPeriod.Year year ->
            let date = new DateTime(year, 1, 1)
            "invoice.InvoiceDate >= @FilterStartDate AND invoice.InvoiceDate < @FilterEndDate", [
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
                    year.IsClosed AS FinancialYearIsClosed,
                    year.Code AS FinancialYearCode,
                    invoice.InvoiceNumber,
                    invoice.Cost,
                    dKey.Name as DistributionKeyName,
                    invoice.OrganizationId,
                    org.Name AS OrganizationName,
                    cat.Code AS CategoryCode,
                    cat.Description AS CategoryDescription,
                    invoice.InvoiceDate,
                    invoice.DueDate,
                    (SELECT SUM(Amount) FROM InvoicePayments payment WHERE invoice.InvoiceId = payment.InvoiceId AND payment.IsDeleted = FALSE) AS PaidAmount
                FROM Invoices invoice
                LEFT JOIN FinancialYears year ON invoice.FinancialYearId = year.FinancialYearId
                LEFT JOIN DistributionKeys dKey ON invoice.DistributionKeyId = dKey.DistributionKeyId
                LEFT JOIN FinancialCategories cat ON invoice.FinancialCategoryId = cat.FinancialCategoryId
                LEFT JOIN Organizations org ON invoice.OrganizationId = org.OrganizationId
                WHERE invoice.BuildingId = @BuildingId AND invoice.IsDeleted = FALSE AND %s
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


let getFinancialYearsByIds (conn: string) (buildingId: BuildingId) (financialYearIds: Guid list): Async<FinancialYear list> =
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
            FROM FinancialYears WHERE BuildingId = @BuildingId AND FinancialYearId = ANY (@FinancialYearIds)
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
                BuildingId,
                Code,
                Description,
                LotOwnerId
            FROM FinancialCategories WHERE BuildingId = @BuildingId AND IsDeleted = FALSE
        """
    |> Sql.parameters [
        "@BuildingId", Sql.uuid buildingId
    ]
    |> Sql.read readFinancialCategory

let getFinancialCategoriesByIds (conn: string) (buildingId: BuildingId) (financialCategoryIds: Guid list): Async<FinancialCategory list> =
    Sql.connect conn
    |> Sql.query 
        """
            SELECT
                FinancialCategoryId,
                BuildingId,
                Code,
                Description,
                LotOwnerId
            FROM FinancialCategories WHERE BuildingId = @BuildingId AND FinancialCategoryId = ANY (@FinancialCategoryIds)
        """
    |> Sql.parameters [
        "@BuildingId", Sql.uuid buildingId
        "@FinancialCategoryIds", Sql.uuidArray (financialCategoryIds |> List.toArray)
    ]
    |> Sql.read readFinancialCategory

type InvoicePaymentDbRow = {
    InvoiceId: Guid
    BuildingId: Guid
    InvoicePaymentId: Guid
    Amount: Decimal
    Date: DateTime
    FromBankAccount: BankAccount
    FinancialCategoryId: Guid
}

let private readInvoicePayment (reader: CaseInsensitiveRowReader): InvoicePaymentDbRow = {
    InvoiceId = reader.uuid "InvoiceId"
    BuildingId = reader.uuid "BuildingId"
    InvoicePaymentId = reader.uuid "InvoicePaymentId"
    Amount = reader.decimal "Amount"
    Date = reader.dateTime "Date"
    FromBankAccount = BankAccount.fromJson (reader.string "FromBankAccount")
    FinancialCategoryId = reader.uuid "FinancialCategoryId"
}

let getInvoicePaymentsByInvoiceIds (conn: string) (buildingId: BuildingId, invoiceIds: Guid list): Async<InvoicePayment list> = async {
    let! payments =
        Sql.connect conn
        |> Sql.query
            """
                SELECT 
                    payment.InvoiceId,
                    payment.BuildingId,
                    payment.InvoicePaymentId,
                    payment.Amount,
                    payment.Date,
                    payment.FromBankAccount,
                    payment.FinancialCategoryId,
                FROM InvoicePayments payment
                WHERE payment.BuildingId = @BuildingId AND payment.IsDeleted = FALSE AND payment.InvoiceId = ANY (@InvoiceIds)
           """
        |> Sql.parameters [
            "@InvoiceIds", Sql.uuidArray (invoiceIds |> List.toArray)
            "@BuildingId", Sql.uuid buildingId
        ]
        |> Sql.read readInvoicePayment
    let! financialCategories = 
        getFinancialCategoriesByIds conn buildingId (payments |> List.map (fun p -> p.FinancialCategoryId) |> List.distinct)
        |> Async.map (List.map (fun cat -> cat.FinancialCategoryId, cat) >> Map.ofList)
    let! mediaFiles = 
        Server.Media.Query.getMediaFilesForEntities conn Partitions.InvoicePayments (payments |> List.map (fun p -> p.InvoicePaymentId))
        |> Async.map (List.groupBy (fun file -> file.EntityId) >> Map.ofList)
    return payments |> List.map (fun dbPayment -> {
        InvoiceId = dbPayment.InvoiceId
        BuildingId = dbPayment.BuildingId
        InvoicePaymentId = dbPayment.InvoicePaymentId
        Amount = dbPayment.Amount
        Date = dbPayment.Date
        FromBankAccount = dbPayment.FromBankAccount
        FinancialCategory = financialCategories |> Map.find (dbPayment.FinancialCategoryId)
        MediaFiles = mediaFiles |> Map.tryFind (dbPayment.InvoicePaymentId) |> Option.defaultValue [] 
    })
}

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
                WHERE InvoiceId = @InvoiceId AND IsDeleted = FALSE
            """
        |> Sql.parameters [ "@InvoiceId", Sql.uuid invoiceId ]
        |> Sql.readSingle readInvoice

    match dbRowOption with
    | Some dbRow ->
        let! financialYear = getFinancialYearsByIds conn dbRow.BuildingId [ dbRow.FinancialYearId ] |> Async.map List.head
        let! financialCategory = getFinancialCategoriesByIds conn dbRow.BuildingId [ dbRow.FinancialCategoryId ] |> Async.map List.head
        let! distributionKey = getDistributionKeysByIds conn (dbRow.BuildingId, [ dbRow.DistributionKeyId ]) |> Async.map List.head
        let! mediaFiles = Server.Media.Query.getMediaFilesForEntities conn (Partitions.Invoices) [ dbRow.InvoiceId ]
        let! organization = Server.Organizations.Query.getOrganizationsByIds conn [ dbRow.OrganizationId ] |> Async.map List.head
        let! payments = getInvoicePaymentsByInvoiceIds conn (dbRow.BuildingId, [ dbRow.InvoiceId ])
        let invoiceDate = dbRow.InvoiceDate
        let dueDate = dbRow.DueDate
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
            InvoiceDate = new DateTimeOffset(invoiceDate.Year, invoiceDate.Month, invoiceDate.Day, 2, 0, 0, TimeSpan.FromHours(2.0))
            DueDate = new DateTimeOffset(dueDate.Year, dueDate.Month, dueDate.Day, 2, 0, 0, TimeSpan.FromHours(2.0))
            MediaFiles = mediaFiles
            Payments = payments
        }
    | None ->
        return None
}

let getFinancialCategoryByLotOwnerId (conn: string) (buildingId: BuildingId, lotOwnerId: Guid) =
    Sql.connect conn
    |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId; "@LotOwnerId", Sql.uuid lotOwnerId ]
    |> Sql.query 
        """
            SELECT FinancialCategoryId, BuildingId, Code, Description, LotOwnerId 
            FROM FinancialCategories
            WHERE BuildingId = @BuildingId AND LotOwnerId = @LotOwnerId
        """
    |> Sql.readSingle readFinancialCategory

let private getFinancialCategoryByCode (conn: string) (buildingId: BuildingId, code: string) =
    Sql.connect conn
    |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId; "@Code", Sql.string code ]
    |> Sql.query
        """
            SELECT FinancialCategoryId, BuildingId, Code, Description, LotOwnerId 
            FROM FinancialCategories
            WHERE BuildingId = @BuildingId AND Code = @Code
        """
    |> Sql.readSingle readFinancialCategory
    |> Async.map (function | None -> failwithf "Failed to find financial category with code %s..." code | Some cat -> cat)

let getNewFinancialCategoryCodesForLotOwner (conn: string) (buildingId: BuildingId, lotOwner: ValidatedLotOwner): Async<string list> = async {
    match! getFinancialCategoryByLotOwnerId conn (buildingId, lotOwner.LotOwnerId) with
    | Some category -> 
        return []
    | None ->
        let! relevantCodes =
            Sql.connect conn
            |> Sql.query
                """
                    SELECT Code FROM
                    FROM FinancialCategories
                    WHERE Code like '400%' OR Code like '4100%' OR Code like '4101%'
                    AND BuildingId = @BuildingId
                """
            |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
            |> Sql.read (fun reader -> reader.string "Code")

        let max400, max4100, max4101 =
            relevantCodes
            |> List.fold (fun (c400, c4100, c4101) next -> 
                if next.StartsWith("400") then next::c400,c4100,c4101
                elif next.StartsWith("4100") then c400,next::c4100,c4101
                else c400,c4100,next::c4101
            ) ([], [], [])
            |> (fun (c400, c4100, c4101) -> c400 |> List.tryMax, c4100 |> List.tryMax, c4101 |> List.tryMax)

        let next400 =
            match max400 with
            | Some max -> 
                match Int32.TryParse(max.Substring(1)) with
                | true, parsed -> 
                    Some (string (parsed + 1 + 40000000))
                | false, _ -> 
                    failwithf "Invalid value for financial category: %s" max
                    None
            | None ->
                Some "40000001"
        let next4100 =
            match max4100 with
            | Some max -> 
                match Int32.TryParse(max.Substring(2)) with
                | true, parsed -> 
                    Some (string (parsed + 1 + 410000000))
                | false, _ -> 
                    failwithf "Invalid value for financial category: %s" max
                    None
            | None ->
                Some "410000001"
        let next4101 =
            match max4101 with
            | Some max -> 
                match Int32.TryParse(max.Substring(3)) with
                | true, parsed -> 
                    Some (string (parsed + 1 + 410100000))
                | false, _ -> 
                    failwithf "Invalid value for financial category: %s" max
                    None
            | None ->
                Some "410100001"

        return [ next400; next4100; next4101 ] |> List.choose id
}

let private invoiceToFinancialTransactions (supplierFinancialCategory: FinancialCategory) (invoice: InvoiceListItem): FinancialTransaction list = [
    {
        FinancialTransactionId = Guid.NewGuid()
        BuildingId = invoice.BuildingId
        Date = invoice.InvoiceDate.DateTime
        Source = Some (Invoice { InvoiceId = invoice.InvoiceId; OrganizationName = invoice.OrganizationName })
        FinancialCategoryCode = invoice.FinancialCategoryCode
        FinancialCategoryDescription = invoice.FinancialCategoryDescription
        Amount = Debit invoice.Cost
        FinancialYearIsClosed = invoice.FinancialYearIsClosed
    }
    {
        FinancialTransactionId = Guid.NewGuid()
        BuildingId = invoice.BuildingId
        Date = invoice.InvoiceDate.DateTime
        Source = Some (Invoice { InvoiceId = invoice.InvoiceId; OrganizationName = invoice.OrganizationName })
        FinancialCategoryCode = supplierFinancialCategory.Code
        FinancialCategoryDescription = supplierFinancialCategory.Description
        Amount = Credit invoice.Cost
        FinancialYearIsClosed = invoice.FinancialYearIsClosed
    }
]

let private invoicePaymentToFinancialTransactions (supplierFinancialCategory: FinancialCategory) (invoice: InvoiceListItem, payment: InvoicePayment): FinancialTransaction list = [
    {
        FinancialTransactionId = Guid.NewGuid()
        BuildingId = payment.BuildingId
        Date = payment.Date
        Source = Some (InvoicePayment { InvoiceId = payment.InvoiceId; InvoicePaymentId = payment.InvoicePaymentId; OrganizationName = invoice.OrganizationName })
        FinancialCategoryCode = supplierFinancialCategory.Code
        FinancialCategoryDescription = supplierFinancialCategory.Description
        Amount = Debit payment.Amount
        FinancialYearIsClosed = invoice.FinancialYearIsClosed
    }
    {
        FinancialTransactionId = Guid.NewGuid()
        BuildingId = payment.BuildingId
        Date = payment.Date
        Source = Some (InvoicePayment { InvoiceId = payment.InvoiceId; InvoicePaymentId = payment.InvoicePaymentId; OrganizationName = invoice.OrganizationName })
        FinancialCategoryCode = payment.FinancialCategory.Code
        FinancialCategoryDescription = payment.FinancialCategory.Description
        Amount = Credit payment.Amount
        FinancialYearIsClosed = invoice.FinancialYearIsClosed
    }
]

let private readDepositRequestListItem (reader: CaseInsensitiveRowReader): DepositRequestListItem = 
    let amount = reader.decimal "Amount"
    {
        BuildingId = reader.uuid "BuildingId"
        DepositRequestId = reader.uuid "DepositRequestId"
        DepositRequestNumber = reader.int "DepositRequestNumber"
        FinancialYearCode = reader.string "FinancialYearCode"
        FinancialYearIsClosed = reader.bool "FinancialYearIsClosed"
        Amount = amount
        RequestDate =
            let requestDate = reader.dateTime "RequestDate"
            new DateTimeOffset (requestDate.Year, requestDate.Month, requestDate.Day, 2, 0, 0, TimeSpan.FromHours(2.0))
        BookingDate =
            let bookingDate = reader.dateTime "BookingDate"
            new DateTimeOffset (bookingDate.Year, bookingDate.Month, bookingDate.Day, 2, 0, 0, TimeSpan.FromHours(2.0))
        DueDate =
            let dueDate = reader.dateTime "DueDate"
            new DateTimeOffset (dueDate.Year, dueDate.Month, dueDate.Day, 2, 0, 0, TimeSpan.FromHours(2.0))
        ToFinancialCategoryCode = reader.string "ToFinancialCategoryCode"
        ToFinancialCategoryDescription = reader.string "ToFinancialCategoryDescription"
        DistributionKeyName = reader.string "DistributionKeyName"
        IsPaid =
            let deposited = 
                reader.decimalOrNone "DepositedAmount" 
                |> Option.defaultValue Decimal.Zero
            amount - deposited = Decimal.Zero        
    }

let getDepositRequests (conn: string) (filter: FinancialTransactionFilter): Async<DepositRequestListItem list> =
    let whereFilter, whereParameters =
        match filter.Period with
        | FinancialTransactionFilterPeriod.FinancialYear financialYearId -> 
            "request.FinancialYearId = @FinancialYearId", [
                "@FinancialYearId", Sql.uuid financialYearId
            ]
        | FinancialTransactionFilterPeriod.Month (month, year) ->
            let date = new DateTime(year, month, 1)
            "request.RequestDate >= @FilterStartDate AND request.RequestDate < @FilterEndDate", [
                "@FilterStartDate", Sql.timestamp date 
                "@FilterEndDate", Sql.timestamp (date.AddMonths(1))
            ]
        | FinancialTransactionFilterPeriod.Year year ->
            let date = new DateTime(year, 1, 1)
            "request.RequestDate >= @FilterStartDate AND request.RequestDate < @FilterEndDate", [
                "@FilterStartDate", Sql.timestamp date
                "@FilterEndDate", Sql.timestamp (date.AddYears(1))
            ]

    Sql.connect conn
    |> Sql.query
        (sprintf
            """
                SELECT
                    request.BuildingId,
                    request.DepositRequestId,
                    request.DepositRequestNumber,
                    year.Code AS FinancialYearCode,
                    year.IsClosed AS FinancialYearIsClosed,
                    request.Amount,
                    request.RequestDate,
                    request.BookingDate,
                    request.DueDate,
                    toCat.Code AS ToFinancialCategoryCode,
                    toCat.Description AS ToFinancialCategoryDescription,
                    (SELECT SUM(Amount) FROM Deposits deposit WHERE request.DepositRequestId = deposit.DepositRequestId AND deposit.IsDeleted = FALSE) AS DepositedAmount,
                    distributionKey.Name AS DistributionKeyName
                FROM DepositRequests request
                LEFT JOIN FinancialYears year ON request.FinancialYearId = year.FinancialYearId
                LEFT JOIN FinancialCategories toCat ON request.ToFinancialCategoryId = toCat.FinancialCategoryId
                LEFT JOIN DistributionKeys distributionKey ON request.DistributionKeyId = distributionKey.DistributionKeyId
                WHERE request.BuildingId = @BuildingId AND request.IsDeleted = FALSE AND %s
            """
            whereFilter)
    |> Sql.parameters ([ "@BuildingId", Sql.uuid filter.BuildingId ] @ whereParameters)
    |> Sql.read readDepositRequestListItem

type DepositRequestDbRow = {
    BuildingId: Guid
    DepositRequestId: Guid
    DepositRequestNumber: int
    FinancialYearId: Guid
    Amount: Decimal
    BookingDate: DateTime
    RequestDate: DateTime
    DueDate: DateTime
    Description: string option
    ToBankAccount: BankAccount
    ToFinancialCategoryId: Guid
    DistributionKeyId: Guid
}

let private readDepositRequest (reader: CaseInsensitiveRowReader): DepositRequestDbRow = {
    BuildingId = reader.uuid "BuildingId"
    DepositRequestId = reader.uuid "DepositRequestId"
    DepositRequestNumber = reader.int "DepositRequestNumber"
    FinancialYearId = reader.uuid "FinancialYearId"
    ToFinancialCategoryId = reader.uuid "ToFinancialCategoryId"
    Amount = reader.decimal "Amount"
    BookingDate = reader.dateTime "BookingDate"
    RequestDate = reader.dateTime "RequestDate"
    DueDate = reader.dateTime "DueDate"
    Description = reader.stringOrNone "Description"
    ToBankAccount = BankAccount.fromJson (reader.string "ToBankAccount")
    DistributionKeyId = reader.uuid "DistributionKeyId"
}

type DepositDbRow = {
    DepositId: Guid
    DepositRequestId: Guid
    BuildingId: Guid
    Amount: Decimal
    Date: DateTime
    FromBankAccount: BankAccount option
    LotOwnerId: Guid
    FromFinancialCategoryId: Guid
    ToFinancialCategoryId: Guid
}

let private readDeposit (reader: CaseInsensitiveRowReader): DepositDbRow = {
    DepositId = reader.uuid "DepositId"
    DepositRequestId = reader.uuid "DepositRequestId"
    BuildingId = reader.uuid "BuildingId"
    Amount = reader.decimal "Amount"
    Date = reader.dateTime "Date"
    FromBankAccount = reader.stringOrNone "FromBankAccount" |> Option.map BankAccount.fromJson
    LotOwnerId = reader.uuid "LotOwnerId"
    FromFinancialCategoryId = reader.uuid "FromFinancialCategoryId"
    ToFinancialCategoryId = reader.uuid "ToFinancialCategoryId"
}

let getDepositsByDepositRequestIds (conn: string) (buildingId: BuildingId, requestIds: Guid list): Async<Deposit list> = async {
    let! deposits =
        Sql.connect conn
        |> Sql.query
            """
                SELECT
                    deposit.LotOwnerId,
                    deposit.DepositId,
                    deposit.DepositRequestId,
                    deposit.BuildingId,
                    deposit.Amount,
                    deposit.Date,
                    deposit.FromBankAccount,
                    deposit.FromFinancialCategoryId,
                    deposit.ToFinancialCategoryId
                FROM Deposits
                WHERE BuildingId = @BuildingId AND IsDeleted = FALSE AND DepositRequestId = ANY(@RequestIds)
            """
        |> Sql.parameters [
            "@BuildingId", Sql.uuid buildingId
            "@RequestIds", Sql.uuidArray (requestIds |> List.toArray)
        ]
        |> Sql.read readDeposit

    let! mediaFiles =
        deposits
        |> List.map (fun d -> d.DepositId)
        |> Server.Media.Query.getMediaFilesForEntities conn Partitions.Deposits
        |> Async.map (List.groupBy (fun file -> file.EntityId) >> Map.ofList)

    let! financialCategories =
        deposits
        |> List.collect (fun d -> [ d.FromFinancialCategoryId; d.ToFinancialCategoryId ])
        |> List.distinct
        |> getFinancialCategoriesByIds conn buildingId
        |> Async.map (List.map (fun cat -> cat.FinancialCategoryId, cat) >> Map.ofList)

    let! lotOwners =
        deposits
        |> List.map (fun d -> d.LotOwnerId)
        |> List.distinct
        |> Server.Lots.Query.getLotOwnersByIds conn
        |> Async.map (List.map (fun o -> o.LotOwnerId, o) >> Map.ofList)

    return deposits |> List.map (fun dbPayment -> {
        Deposit.DepositId = dbPayment.DepositId
        Deposit.DepositRequestId = dbPayment.DepositRequestId
        Deposit.BuildingId = dbPayment.BuildingId
        Deposit.Amount = dbPayment.Amount
        Deposit.Date = dbPayment.Date
        Deposit.FromBankAccount = dbPayment.FromBankAccount
        Deposit.MediaFiles = mediaFiles |> Map.tryFind (dbPayment.DepositRequestId) |> Option.defaultValue [] 
        Deposit.LotOwner = lotOwners |> Map.find (dbPayment.LotOwnerId)
        Deposit.FromFinancialCategory = financialCategories |> Map.find (dbPayment.FromFinancialCategoryId)
        Deposit.ToFinancialCategory = financialCategories |> Map.find (dbPayment.ToFinancialCategoryId)
    })
}

let getDepositRequest (conn: string) (requestId: Guid): Async<DepositRequest option> = async {   
    let! dbRowOption =
        Sql.connect conn
        |> Sql.query
            """ 
                SELECT
                    request.BuildingId,
                    request.DepositRequestId,
                    request.DepositRequestNumber,
                    request.FinancialYearId,
                    request.Amount,
                    request.BookingDate,
                    request.RequestDate,
                    request.DueDate,
                    request.Description,
                    request.ToBankAccount,
                    request.DistributionKeyId,
                    request.ToFinancialCategoryId
                FROM DepositRequests request
                WHERE request.DepositRequestId = @DepositRequestId AND request.IsDeleted = FALSE
            """
        |> Sql.parameters [ "@DepositRequestId", Sql.uuid requestId ]
        |> Sql.readSingle readDepositRequest

    match dbRowOption with
    | Some dbRow ->
        let! financialYear = getFinancialYearsByIds conn dbRow.BuildingId [ dbRow.FinancialYearId ] |> Async.map List.head
        let! financialCategories = getFinancialCategoriesByIds conn dbRow.BuildingId [ dbRow.ToFinancialCategoryId ]
        let toFinancialCategory = financialCategories |> List.find (fun cat -> cat.FinancialCategoryId = dbRow.ToFinancialCategoryId)
        let! mediaFiles = Server.Media.Query.getMediaFilesForEntities conn (Partitions.DepositRequests) [ dbRow.DepositRequestId ]
        let! deposits = getDepositsByDepositRequestIds conn (dbRow.BuildingId, [ dbRow.DepositRequestId ])
        let! distributionKey = getDistributionKeysByIds conn (dbRow.BuildingId, [ dbRow.DistributionKeyId ]) |> Async.map List.head
        let dueDate = dbRow.DueDate
        let requestDate = dbRow.RequestDate
        let creationDate = dbRow.BookingDate
        return Some {
            DepositRequest.DepositRequestId = dbRow.DepositRequestId
            DepositRequest.DepositRequestNumber = dbRow.DepositRequestNumber
            DepositRequest.BuildingId = dbRow.BuildingId
            DepositRequest.Description = dbRow.Description
            DepositRequest.FinancialYear = financialYear
            DepositRequest.ToBankAccount = dbRow.ToBankAccount
            DepositRequest.Amount = dbRow.Amount
            //DepositRequest.Reference = BelgianOGMReference dbRow.Reference
            DepositRequest.ToFinancialCategory = toFinancialCategory
            DepositRequest.RequestDate = new DateTimeOffset(requestDate.Year, requestDate.Month, requestDate.Day, 2, 0, 0, TimeSpan.FromHours(2.0))
            DepositRequest.BookingDate = new DateTimeOffset(creationDate.Year, creationDate.Month, creationDate.Day, 2, 0, 0, TimeSpan.FromHours(2.0))
            DepositRequest.DueDate = new DateTimeOffset(dueDate.Year, dueDate.Month, dueDate.Day, 2, 0, 0, TimeSpan.FromHours(2.0))
            DepositRequest.MediaFiles = mediaFiles
            DepositRequest.Deposits = deposits
            DepositRequest.DistributionKey = distributionKey
        }
    | None ->
        return None
}

let getFinancialTransactions (conn: string) (filter: FinancialTransactionFilter): Async<FinancialTransaction list> = async {
    let! supplierFinancialCategory = getFinancialCategoryByCode conn (filter.BuildingId, "440")
    let! invoices = getInvoices conn filter
    let invoicesMap = invoices |> List.map (fun invoice -> invoice.InvoiceId, invoice) |> Map.ofList
    let! payments = getInvoicePaymentsByInvoiceIds conn (filter.BuildingId, invoices |> List.map (fun invoice -> invoice.InvoiceId))
    let! depositRequests = getDepositRequests conn filter
    let depositRequestsMap = depositRequests |> List.map (fun req -> req.DepositRequestId, req) |> Map.ofList
    let! deposits = getDepositsByDepositRequestIds conn (filter.BuildingId, depositRequests |> List.map (fun req -> req.DepositRequestId))

    let invoiceTransactions =
        invoices
        |> List.collect (invoiceToFinancialTransactions supplierFinancialCategory)
    let invoicePaymentTransactions =
        payments
        |> List.map (fun payment -> invoicesMap |> Map.find payment.InvoiceId, payment)
        |> List.collect (invoicePaymentToFinancialTransactions supplierFinancialCategory)
    //let depositRequestTransactions =
    //    depositRequests
    //    |> List.map (depositRequestToFinancialTransactions )

    return invoiceTransactions @ invoicePaymentTransactions
}