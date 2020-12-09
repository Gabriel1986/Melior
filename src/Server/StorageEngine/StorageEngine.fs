module Server.StorageEngine.StorageEngine

open Server.Library
open Server.Blueprint.Behavior.Storage
open Server.Blueprint.Data.Storage
open Server.PostgreSQL
open Server.AppSettings
open Npgsql.FSharp
open Microsoft.Extensions.Configuration

let transformEventToSqlStmts (msg: Message<StorageEvent>) =
    match msg.Payload with
    | StorageEvent.BuildingEvent buildingEvent ->
        buildingEvent |> inMsg msg |> BuildingStorage.transformEventToSql
    | StorageEvent.ContractEvent contractEvent ->
        contractEvent |> inMsg msg |> ContractStorage.transformEventToSql
    | StorageEvent.LotEvent lotEvent ->
        lotEvent |> inMsg msg |> LotStorage.transformEventToSql
    | StorageEvent.FinancialEvent financialEvent ->
        financialEvent |> inMsg msg |> FinancialStorage.transformEventToSql
    | StorageEvent.ProfessionalSyndicEvent proSyndicEvent ->
        proSyndicEvent |> inMsg msg |> ProfessionalSyndicStorage.transformEventToSql
    | StorageEvent.MediaEvent mediaEvent ->
        mediaEvent |> inMsg msg |> MediaStorage.transformEventToSql
    | StorageEvent.OrganizationEvent orgEvent ->
        orgEvent |> inMsg msg |> OrganizationStorage.transformEventToSql
    | StorageEvent.OwnerEvent ownerEvent ->
        ownerEvent |> inMsg msg |> OwnerStorage.transformEventToSql
    | StorageEvent.UserEvent userEvent ->
        userEvent |> inMsg msg |> UserStorage.transformEventToSql

let persistTransactional (conn: string) (msgs: Message<StorageEvent> list) = async {
    let! result =
        conn
        |> Sql.connect
        |> Sql.executeTransactionAsync (List.collect transformEventToSqlStmts msgs)

    return
        match result with
        | Ok counts -> counts |> List.tryHead |> Option.defaultValue 0
        | Error e -> raise e
}

type StorageEngine (config: IConfiguration, txReactiveBehaviors: IReactiveBehavior list) =
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection

    let reactTo (events: Message<StorageEvent> list) = async {
        let! reactions =
            [ for b in txReactiveBehaviors do
                for e in events do
                    async {
                        let! reactions = b.ReactTo e
                        return reactions |> List.map (fun r -> r |> inMsg e )
                    } ]
            |> Async.Sequential

        return Array.fold List.append [] reactions
    }

    let triggerChainReaction (events: Message<StorageEvent> list): Async<Message<StorageEvent> list> =
        let rec go events acc = async {
            match! reactTo events with
            | [] -> return acc @ events
            | reaction -> return! go reaction (acc @ events)
        }
        go events []

    let persistTransactional (events: Message<StorageEvent> list) = async {        
        let! events = triggerChainReaction events
        return! persistTransactional conn events
    }

    interface IStorageEngine with
        member _.PersistTransactional (events: Message<StorageEvent> list) =
            persistTransactional events