module Server.StorageEngine.FinancialStorage

open System
open Npgsql.FSharp
open Shared.Read
open Shared.Write
open Server.Library
open Server.LibraryExtensions
open Server.PostgreSQL
open Server.Blueprint.Data.Financial
open Server.Blueprint.Data.Storage

let private paramsForDistributionKey (msg: Message<ValidatedDistributionKey>) =
    let validated = msg.Payload
    let currentUser = msg.CurrentUser
    [
        "@DistributionKeyId", Sql.uuid validated.DistributionKeyId
        "@BuildingId", Sql.uuidOrNone validated.BuildingId
        "@Name", Sql.string (string validated.Name)
        "@DistributionType", Sql.string (string validated.DistributionType)
        "@CreatedBy", Sql.string (currentUser.Principal ())
        "@CreatedAt", Sql.timestamp DateTime.UtcNow
        "@LastUpdatedBy", Sql.string (currentUser.Principal ())
        "@LastUpdatedAt", Sql.timestamp DateTime.UtcNow
    ]

let private paramsForInvoice (msg: Message<ValidatedInvoice>) =
    let invoice = msg.Payload
    let currentUser = msg.CurrentUser
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
           "@InvoiceDate", Sql.timestamp (invoice.InvoiceDate.AddHours(2.0).Date)
           "@DueDate", Sql.timestamp (invoice.DueDate.AddHours(2.0).Date)
           "@CreatedBy", Sql.string (currentUser.Principal ())
           "@CreatedAt", Sql.timestamp DateTime.UtcNow
           "@LastUpdatedBy", Sql.string (currentUser.Principal ())
           "@LastUpdatedAt", Sql.timestamp (DateTime.UtcNow)
       ]

let private paramsForFinancialYear (year: ValidatedFinancialYear) = [
    "@FinancialYearId", Sql.uuid year.FinancialYearId
    "@BuildingId", Sql.uuid year.BuildingId
    "@Code", Sql.string (string year.Code)
    "@StartDate", Sql.timestamp year.StartDate
    "@EndDate", Sql.timestamp year.EndDate
]

let private paramsForFinancialCategory (category: ValidatedFinancialCategory) = [
    "@FinancialCategoryId", Sql.uuid category.FinancialCategoryId
    "@BuildingId", Sql.uuid category.BuildingId
    "@Code", Sql.string (string category.Code)
    "@Description", Sql.string (string category.Description)
]

let private setLotsOrLotTypesFor (distributionKey: ValidatedDistributionKey) =
    let distributionKeyId = distributionKey.DistributionKeyId

    let deleteAll =
        "DELETE FROM DistributionKeyLotsOrLotTypes WHERE DistributionKeyId = @DistributionKeyId"
        , [[ "@DistributionKeyId", Sql.uuid distributionKeyId ]]

    let insertAll =
        match distributionKey.LotsOrLotTypes with
        | Lots lotIds ->
            """
                INSERT INTO DistributionKeyLotsOrLotTypes (DistributionKeyId, LotId) values (@DistributionKeyId, @LotId)
            """, lotIds |> List.map (fun lotId -> [
                "@DistributionKeyId", Sql.uuid distributionKeyId 
                "@LotId", Sql.uuid lotId
            ])
        | LotTypes lotTypes ->
            """
                INSERT INTO DistributionKeyLotsOrLotTypes (DistributionKeyId, LotType) values (@DistributionKeyId, @LotType)
            """, lotTypes |> List.map (fun lotType -> [
                "@DistributionKeyId", Sql.uuid distributionKeyId 
                "@LotType", Sql.string (string lotType)
            ])

    deleteAll::[ insertAll ]

let private writeHistoricalInvoiceEntry (invoiceId: Guid, buildingId: Guid) = [
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
    , [[
        "@InvoiceId", Sql.uuid invoiceId
        "@BuildingId", Sql.uuid buildingId
    ]]
]

let transformEventToSql (msg: Message<FinancialEvent>) =
    match msg.Payload with
    | FinancialEvent.DistributionKeyEvent event ->
        match event with
        | BuildingSpecificCUDEvent.Created distributionKey ->
            [
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
                """, [ distributionKey |> inMsg msg |> paramsForDistributionKey ]
            ] @ setLotsOrLotTypesFor distributionKey
        | BuildingSpecificCUDEvent.Updated distributionKey ->
            [
            """
                UPDATE DistributionKeys SET
                    Name = @Name,
                    DistributionType = @DistributionType,
                    LastUpdatedBy = @LastUpdatedBy,
                    LastUpdatedAt = @LastUpdatedAt
                WHERE
                    DistributionKeyId = @DistributionKeyId AND BuildingId = @BuildingId
            """, [ distributionKey |> inMsg msg |> paramsForDistributionKey ]
            ] @ setLotsOrLotTypesFor distributionKey
        | BuildingSpecificCUDEvent.Deleted (buildingId: BuildingId, distributionKeyId: Guid) ->
            [
                """
                    UPDATE DistributionKeys
                    SET 
                        IsActive = FALSE,
                        LastUpdatedBy = @LastUpdatedBy,
                        LastUpdatedAt = @LastUpdatedAt
                    WHERE DistributionKeyId = @DistributionKeyId
                    AND BuildingId = @BuildingId
                """, [[
                    "@LastUpdatedBy", Sql.string (msg.CurrentUser.Principal ())
                    "@LastUpdatedAt", Sql.timestamp DateTime.UtcNow
                    "@DistributionKeyId", Sql.uuid distributionKeyId 
                    "@BuildingId", Sql.uuid buildingId
                ]]
            ]
    | InvoiceEvent event ->
        match event with
        | BuildingSpecificCUDEvent.Created validated ->
            [
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
                """, [ validated |> inMsg msg |> paramsForInvoice ]
            ] @ writeHistoricalInvoiceEntry (validated.InvoiceId, validated.BuildingId)
        | BuildingSpecificCUDEvent.Updated validated ->
            [
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
                """, [ validated |> inMsg msg |> paramsForInvoice ]
            ] @ writeHistoricalInvoiceEntry (validated.InvoiceId, validated.BuildingId)
        | BuildingSpecificCUDEvent.Deleted (buildingId: BuildingId, invoiceId: Guid) ->
            [
                """
                    UPDATE Invoices 
                    SET 
                        IsActive = FALSE,
                        LastUpdatedBy = @LastUpdatedBy,
                        LastUpdatedAt = @LastUpdatedAt
                    WHERE InvoiceId = @InvoiceId AND BuildingId = @BuildingId
                """, [[
                    "@InvoiceId", Sql.uuid invoiceId
                    "@BuildingId", Sql.uuid buildingId
                ]]
            ] @ writeHistoricalInvoiceEntry (invoiceId, buildingId)
    | FinancialCategoryEvent event ->
        match event with
        | BuildingSpecificCUDEvent.Created validated ->
            [
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
                """, [ paramsForFinancialCategory validated ]
            ]
        | BuildingSpecificCUDEvent.Updated validated ->
            [
                """
                    UPDATE FinancialCategories
                    SET
                        Code = @Code,
                        Description = @Description
                    WHERE FinancialCategoryId = @FinancialCategoryId AND BuildingId = @BuildingId
                """, [ paramsForFinancialCategory validated ]
            ]
        | BuildingSpecificCUDEvent.Deleted (buildingId: BuildingId, financialCategoryId: Guid) ->
            [
                """
                    Update FinancialCategories
                    SET IsDeleted = TRUE
                    Where FinancialCategoryId = @FinancialCategoryId AND BuildingId = @BuildingId
                """, [[
                    "@BuildingId", Sql.uuid buildingId
                    "FinancialCategoryId", Sql.uuid financialCategoryId
                ]]
            ]
    | FinancialYearEvent event ->
        match event with
        | BuildingSpecificCUDEvent.Created validated ->
            [
                """
                    INSERT INTO FinancialYears (
                        FinancialYearId,
                        BuildingId,
                        Code,
                        StartDate,
                        EndDate
                    ) VALUES (
                        @FinancialYearId,
                        @BuildingId,
                        @Code,
                        @StartDate,
                        @EndDate
                    )
                """, [ paramsForFinancialYear validated ]
            ]
        | BuildingSpecificCUDEvent.Updated validated ->
            [
                """
                    UPDATE FinancialYears
                    SET
                        Code = @Code,
                        StartDate = @StartDate,
                        EndDate = @EndDate
                    WHERE FinancialYearId = @FinancialYearId AND BuildingId = @BuildingId
                """, [ paramsForFinancialYear validated ]
            ]
        | BuildingSpecificCUDEvent.Deleted (buildingId, financialYearId) ->
            [
                """
                    Update FinancialYears
                    SET IsDeleted = TRUE
                    Where FinancialYearId = @FinancialYearId AND BuildingId = @BuildingId
                """, [[
                    "@BuildingId", Sql.uuid buildingId
                    "FinancialYearId", Sql.uuid financialYearId
                ]]
            ]
    | FinancialYearWasClosed validated ->
        //TODO: so many things need to happen as reactive behavior
        [
            """
                UPDATE FinancialYears
                SET IsClosed = TRUE
                WHERE FinancialYearId = @FinancialYearId AND BuildingId = @BuildingId
            """, [[ "@BuildingId", Sql.uuid validated.BuildingId; "FinancialYearId", Sql.uuid validated.FinancialYearId ]]
        ]