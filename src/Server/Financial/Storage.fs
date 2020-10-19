module Server.Financial.Storage

open System
open Shared.Read
open Shared.Write
open Npgsql.FSharp
open Server.PostgreSQL
open Server.Library
open Server.LibraryExtensions

type IFinancialStorage =
    abstract CreateDistributionKey: Message<ValidatedDistributionKey> -> Async<unit>
    abstract UpdateDistributionKey: Message<ValidatedDistributionKey> -> Async<int>
    abstract DeleteDistributionKey: Message<BuildingId * Guid> -> Async<int>

    abstract CreateInvoice: Message<ValidatedInvoice> -> Async<unit>
    abstract UpdateInvoice: Message<ValidatedInvoice> -> Async<int>
    abstract DeleteInvoice: Message<BuildingId * Guid> -> Async<int>

    abstract CreateFinancialYear: Message<ValidatedFinancialYear> -> Async<unit>
    abstract UpdateFinancialYear: Message<ValidatedFinancialYear> -> Async<int>
    abstract DeleteFinancialYear: Message<BuildingId * Guid> -> Async<int>

    abstract CreateFinancialCategory: Message<ValidatedFinancialCategory> -> Async<unit>
    abstract UpdateFinancialCategory: Message<ValidatedFinancialCategory> -> Async<int>
    abstract DeleteFinancialCategory: Message<BuildingId * Guid> -> Async<int>

let private generateSqlForLotsOrLotTypes (distributionKeyId: Guid, lotsOrLotTypes: LotsOrLotTypes) =
    match lotsOrLotTypes with
    | Lots lotIds ->
        lotIds 
        |> List.map (fun lotId ->
            """
                INSERT INTO DistributionKeyLotsOrLotTypes (DistributionKeyId, LotId) values (@DistributionKeyId, @LotId)
            """, [ 
                "@DistributionKeyId", Sql.uuid distributionKeyId 
                "@LotId", Sql.uuid lotId
            ])
    | LotTypes lotTypes ->       
        lotTypes
        |> List.map (fun lotType ->
            """
                INSERT INTO DistributionKeyLotsOrLotTypes (DistributionKeyId, LotType) values (@DistributionKeyId, @LotType)
            """, [
                "@DistributionKeyId", Sql.uuid distributionKeyId 
                "@LotType", Sql.string (string lotType)
            ])

let private paramsForDistributionKey (msg: Message<ValidatedDistributionKey>) =
    let validated = msg.Payload
    [
        "@DistributionKeyId", Sql.uuid validated.DistributionKeyId
        "@BuildingId", Sql.uuidOrNone validated.BuildingId
        "@Name", Sql.string (string validated.Name)
        "@DistributionType", Sql.string (string validated.DistributionType)
        "@CreatedBy", Sql.string (msg.CurrentUser.Principal ())
        "@CreatedAt", Sql.timestamp DateTime.UtcNow
        "@UpdatedBy", Sql.string (msg.CurrentUser.Principal ())
        "@UpdatedAt", Sql.timestamp DateTime.UtcNow
    ]

let private paramsForInvoice (msg: Message<ValidatedInvoice>) =
    let invoice = msg.Payload
    [
           "@InvoiceId", Sql.uuid invoice.InvoiceId
           "@BuildingId", Sql.uuid invoice.BuildingId
           "@FinancialYearId", Sql.uuid invoice.FinancialYearId
           "@Description", Sql.stringOrNone invoice.Description
           "@Cost", Sql.decimal invoice.Cost
           "@VatRate", Sql.int (invoice.VatRate.Value ())
           "@FinancialCategoryId", Sql.string (string invoice.FinancialCategoryId)
           "@BookingDate", Sql.timestamp invoice.BookingDate
           "@DistributionKeyId", Sql.uuid invoice.DistributionKeyId
           "@OrganizationId", Sql.uuid invoice.OrganizationId
           "@OrganizationBankAccount", Sql.jsonb (ValidatedBankAccount.toJson invoice.OrganizationBankAccount)
           "@OrganizationInvoiceNumber", Sql.stringOrNone (invoice.OrganizationInvoiceNumber |> Option.map string)
           "@InvoiceDate", Sql.timestamp invoice.InvoiceDate
           "@DueDate", Sql.timestampOrNone invoice.DueDate
           "@CreatedBy", Sql.string (msg.CurrentUser.Principal ())
           "@CreatedAt", Sql.timestamp DateTime.UtcNow
           "@LastUpdatedBy", Sql.string (msg.CurrentUser.Principal ())
           "@LastUpdatedAt", Sql.timestamp (DateTime.UtcNow)
       ]

let createDistributionKey (conn) (msg: Message<ValidatedDistributionKey>) =
    let validated = msg.Payload
    Sql.connect conn
    |> Sql.writeBatchAsync [
        yield
            """
                INSERT INTO DistributionKeys (
                    DistributionKeyId,
                    BuildingId,
                    Name,
                    DistributionType,
                    CreatedBy,
                    CreatedAt,
                    LastUpdatedBy,
                    LastUpdatedAt
                ) VALUES (
                    @DistributionKeyId,
                    @BuildingId,
                    @Name,
                    @DistributionType,
                    @CreatedBy,
                    @CreatedAt,
                    @LastUpdatedBy,
                    @LastUpdatedAt
                )
            """, paramsForDistributionKey msg
        yield!
            generateSqlForLotsOrLotTypes (validated.DistributionKeyId, validated.LotsOrLotTypes)
    ]
    |> Async.Ignore

let updateDistributionKey (conn) (msg: Message<ValidatedDistributionKey>) =
    let validated = msg.Payload
    Sql.connect conn
    |> Sql.writeBatchAsync [
        yield
            """
                UPDATE DistributionKeys SET
                    Name = @Name,
                    DistributionType = @DistributionType,
                    LastUpdatedBy = @LastUpdatedBy,
                    LastUpdatedAt = @LastUpdatedAt
                WHERE
                    DistributionKeyId = @DistributionKeyId AND BuildingId = @BuildingId
            """, 
            paramsForDistributionKey msg
        yield
            "DELETE FROM DistributionKeyLotsOrLotTypes WHERE DistributionKeyId = @DistributionKeyId", 
            [ "@DistributionKeyId", Sql.uuid validated.DistributionKeyId ]
        yield!
            generateSqlForLotsOrLotTypes (validated.DistributionKeyId, validated.LotsOrLotTypes)
    ]
    |> Async.map (List.tryHead >> Option.defaultValue 0)

let deleteDistributionKey (conn) (msg: Message<BuildingId * Guid>) =
    let buildingId = fst msg.Payload
    let distributionKeyId = snd msg.Payload
    Sql.connect conn
    |> Sql.query
        """
            UPDATE DistributionKeys
            SET 
                IsActive = FALSE,
                LastUpdatedBy = @LastUpdatedBy,
                LastUpdatedAt = @LastUpdatedAt
            WHERE DistributionKeyId = @DistributionKeyId
            AND BuildingId = @BuildingId
        """
    |> Sql.parameters [
        "@LastUpdatedBy", Sql.string (msg.CurrentUser.Principal ())
        "@LastUpdatedAt", Sql.timestamp DateTime.UtcNow
        "@DistributionKeyId", Sql.uuid distributionKeyId 
        "@BuildingId", Sql.uuid buildingId
    ]
    |> Sql.writeAsync

let createInvoice (conn: string) (msg: Message<ValidatedInvoice>) =
    Sql.connect conn
    |> Sql.query
        """
            INSERT INTO Invoices (
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
                DueDate,
                CreatedBy,
                CreatedAt,
                LastUpdatedBy,
                LastUpdatedAt
            ) VALUES (
                @InvoiceId,
                @BuildingId,
                @FinancialYearId,
                (SELECT (coalesce(MAX(InvoiceNumber),0) + 1) FROM Invoices WHERE FinancialYearId = @FinancialYearId),
                @Description,
                @Cost,
                @VatRate,
                @FinancialCategoryId,
                @BookingDate,
                @DistributionKeyId,
                @OrganizationId,
                @OrganizationBankAccount,
                @OrganizationInvoiceNumber,
                @InvoiceDate,
                @DueDate,
                @CreatedBy,
                @CreatedAt,
                @LastUpdatedBy,
                @LastUpdatedAt
            )
        """
    |> Sql.parameters (paramsForInvoice msg)
    |> Sql.writeAsync
    |> Async.Ignore

let private writeHistoricalInvoiceEntryQuery (invoiceId: Guid, buildingId: Guid) =
    """
        INSERT INTO Invoices_History (
            InvoiceId,
            BuildingId,
            FinancialYearId,
            InvoiceNumber,
            Description,
            Cost,
            VatRate,
            FinancialCategoryId,
            DistributionKeyId,
            OrganizationId,
            OrganizationBankAccount,
            OrganizationInvoiceNumber,
            BookingDate,
            InvoiceDate,
            DueDate,
            LastUpdatedBy,
            LastUpdatedAt
        )
        SELECT (
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
            DueDate,
            LastUpdatedBy,
            LastUpdatedAt
        ) FROM Invoices WHERE InvoiceId = @InvoiceId AND BuildingId = @BuildingId
    """
    , [ 
        "@InvoiceId", Sql.uuid invoiceId
        "@BuildingId", Sql.uuid buildingId
    ]

let updateInvoice (conn: string) (msg: Message<ValidatedInvoice>) =
    let invoice = msg.Payload

    Sql.connect conn
    |> Sql.writeBatchAsync [
        writeHistoricalInvoiceEntryQuery (invoice.InvoiceId, invoice.BuildingId)

        """
            UPDATE Invoices SET
                Description = @Description,
                Cost = @Cost,
                VatRate = @VatRate,
                FinancialCategoryId = @FinancialCategoryId,
                BookingDate = @BookingDate,
                DistributionKeyId = @DistributionKeyId,
                OrganizationId = @OrganizationId,
                OrganizationBankAccount = @OrganizationBankAccount,
                OrganizationInvoiceNumber = @OrganizationInvoiceNumber,
                InvoiceDate = @InvoiceDate,
                DueDate = @DueDate,
                LastUpdatedBy = @LastUpdatedBy,
                LastUpdatedAt = @LastUpdatedAt
            WHERE InvoiceId = @InvoiceId AND BuildingId = @BuildingId
        """, paramsForInvoice msg
    ]
    |> Async.map (List.skip 1 >> List.head)

let deleteInvoice (conn: string) (msg: Message<BuildingId * Guid>) =
    Sql.connect conn
    |> Sql.writeBatchAsync [
        writeHistoricalInvoiceEntryQuery (snd msg.Payload, fst msg.Payload)
    
        """
            UPDATE Invoices 
            SET 
                IsActive = FALSE,
                LastUpdatedBy = @LastUpdatedBy,
                LastUpdatedAt = @LastUpdatedAt
            WHERE InvoiceId = @InvoiceId AND BuildingId = @BuildingId
        """, [
            "@InvoiceId", Sql.uuid (snd msg.Payload)
            "@BuildingId", Sql.uuid (fst msg.Payload)
        ]
    ]
    |> Async.map (List.skip 1 >> List.head)

let private paramsForFinancialYear (year: ValidatedFinancialYear) = [
    "@FinancialYearId", Sql.uuid year.FinancialYearId
    "@BuildingId", Sql.uuid year.BuildingId
    "@Code", Sql.string (string year.Code)
    "@StartDate", Sql.timestampOrNone year.StartDate
    "@EndDate", Sql.timestampOrNone year.EndDate
    "@IsClosed", Sql.bool year.IsClosed
]

let private paramsForFinancialCategory (category: ValidatedFinancialCategory) = [
    "@FinancialCategoryId", Sql.uuid category.FinancialCategoryId
    "@BuildingId", Sql.uuid category.BuildingId
    "@Code", Sql.string (string category.Code)
    "@Description", Sql.string (string category.Description)
]

let createFinancialYear (conn: string) (msg: Message<ValidatedFinancialYear>): Async<unit> =
    let year = msg.Payload
    Sql.connect conn
    |> Sql.query
        """
            INSERT INTO FinancialYears (
                FinancialYearId,
                BuildingId,
                Code,
                StartDate,
                EndDate,
                IsClosed
            ) VALUES (
                @FinancialYearId,
                @BuildingId,
                @Code,
                @StartDate,
                @EndDate,
                @IsClosed
            )
        """
    |> Sql.parameters (paramsForFinancialYear year)
    |> Sql.writeAsync
    |> Async.Ignore

let updateFinancialYear (conn: string) (msg: Message<ValidatedFinancialYear>): Async<int> =
    let year = msg.Payload
    Sql.connect conn
    |> Sql.query
        """
            UPDATE FinancialYears
            SET
                Code = @Code,
                StartDate = @StartDate,
                EndDate = @EndDate,
                IsClosed = @IsClosed
            WHERE FinancialYearId = @FinancialYearId AND BuildingId = @BuildingId
        """
    |> Sql.parameters (paramsForFinancialYear year)
    |> Sql.writeAsync

let deleteFinancialYear (conn: string) (msg: Message<BuildingId * Guid>): Async<int> =
    Sql.connect conn
    |> Sql.query
        """
            Update FinancialYears
            SET IsDeleted = TRUE
            Where FinancialYearId = @FinancialYearId AND BuildingId = @BuildingId
        """
    |> Sql.parameters [
        "@BuildingId", Sql.uuid (fst msg.Payload)
        "FinancialYearId", Sql.uuid (snd msg.Payload)
    ]
    |> Sql.writeAsync

let createFinancialCategory (conn: string) (msg: Message<ValidatedFinancialCategory>): Async<unit> =
    let category = msg.Payload
    Sql.connect conn
    |> Sql.query
        """
            INSERT INTO FinancialCategories (
                FinancialCategoryId,
                BuildingId,
                Code,
                Description
            ) VALUES (
                @FinancialCategoryId,
                @BuildingId,
                @Code,
                @Description
            )
        """
    |> Sql.parameters (paramsForFinancialCategory category)
    |> Sql.writeAsync
    |> Async.Ignore

let updateFinancialCategory (conn: string) (msg: Message<ValidatedFinancialCategory>): Async<int> =
    let category = msg.Payload
    Sql.connect conn
    |> Sql.query
        """
            UPDATE FinancialCategories
            SET
                Code = @Code,
                Description = @Description
            WHERE FinancialCategoryId = @FinancialCategoryId AND BuildingId = @BuildingId
        """
    |> Sql.parameters (paramsForFinancialCategory category)
    |> Sql.writeAsync

let deleteFinancialCategory (conn: string) (msg: Message<BuildingId * Guid>): Async<int> =
    Sql.connect conn
    |> Sql.query
        """
            Update FinancialCategories
            SET IsDeleted = TRUE
            Where FinancialCategoryId = @FinancialCategoryId AND BuildingId = @BuildingId
        """
    |> Sql.parameters [
        "@BuildingId", Sql.uuid (fst msg.Payload)
        "FinancialCategoryId", Sql.uuid (snd msg.Payload)
    ]
    |> Sql.writeAsync

let makeStorage conn = {
    new IFinancialStorage with
        member _.CreateDistributionKey msg = createDistributionKey conn msg
        member _.UpdateDistributionKey msg = updateDistributionKey conn msg
        member _.DeleteDistributionKey msg = deleteDistributionKey conn msg

        member _.CreateInvoice msg = createInvoice conn msg
        member _.UpdateInvoice msg = updateInvoice conn msg
        member _.DeleteInvoice msg = deleteInvoice conn msg

        member _.CreateFinancialYear msg = createFinancialYear conn msg
        member _.UpdateFinancialYear msg = updateFinancialYear conn msg
        member _.DeleteFinancialYear msg = deleteFinancialYear conn msg

        member _.CreateFinancialCategory msg = createFinancialCategory conn msg
        member _.UpdateFinancialCategory msg = updateFinancialCategory conn msg
        member _.DeleteFinancialCategory msg = deleteFinancialCategory conn msg
}