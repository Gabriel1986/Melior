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

let private paramsFor (msg: Message<ValidatedDistributionKey>) =
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
            """, paramsFor msg
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
            paramsFor msg
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

let mutable invoices: ValidatedInvoice list = []
let createInvoice (conn: string) (msg: Message<ValidatedInvoice>) =
    invoices <- msg.Payload::invoices
    Async.lift ()

let updateInvoice (conn: string) (msg: Message<ValidatedInvoice>) =
    invoices <- invoices |> List.map (fun invoice -> if invoice.InvoiceId = msg.Payload.InvoiceId then msg.Payload else invoice)
    Async.lift 1

let deleteInvoice (conn: string) (msg: Message<BuildingId * Guid>) =
    invoices <- invoices |> List.filter (fun invoice -> invoice.InvoiceId = snd msg.Payload)
    Async.lift 1

let makeStorage conn = {
    new IFinancialStorage with
        member _.CreateDistributionKey msg = createDistributionKey conn msg
        member _.UpdateDistributionKey msg = updateDistributionKey conn msg
        member _.DeleteDistributionKey msg = deleteDistributionKey conn msg

        member _.CreateInvoice msg = createInvoice conn msg
        member _.UpdateInvoice msg = updateInvoice conn msg
        member _.DeleteInvoice msg = deleteInvoice conn msg
}