module Server.Lots.LotSystem

open Microsoft.Extensions.Configuration
open Server.AppSettings
open Server.Blueprint.Behavior.Lots
open Server.LibraryExtensions
open Server.Blueprint.Behavior.Storage
open Shared.Library

let build (config: IConfiguration) (store: IStorageEngine): ILotSystem =
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection
    {
        new ILotSystem with
            member _.CreateLot msg = Workflow.createLot store msg   
            member _.UpdateLot msg = Workflow.updateLot store (Query.getLot conn) msg
            member _.DeleteLot msg = Workflow.deleteLot store msg
            member _.GetLot msg = async {
                match! Query.getLot conn msg.Payload with
                | Some lot when msg.CurrentUser.HasAccessToBuilding lot.BuildingId ->
                    return Some lot
                | _ ->
                    return None
            }
            member _.GetLots msg =
                match msg.CurrentUser.HasAccessToBuilding msg.Payload with
                | true -> Query.getLots conn msg.Payload
                | false -> Async.lift []

            member _.GetFinancialLotOwners msg =
                match msg.CurrentUser.HasAccessToBuilding msg.Payload.BuildingId with
                | true -> Query.getLotOwners conn msg.Payload
                | false -> Async.lift []

            member _.GenerateOGMReferences () =
                Workflow.generateOGMReferences store (Query.getAllLotOwners conn)
    }