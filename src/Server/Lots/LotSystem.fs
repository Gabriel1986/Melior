﻿module Server.Lots.LotSystem

open Microsoft.Extensions.Configuration
open Server.AppSettings
open Server.Blueprint.Behavior.Lots
open Server.LibraryExtensions
open Server.Blueprint.Behavior.Storage

let build (config: IConfiguration) (store: IStorageEngine): ILotSystem =
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection
    {
        new ILotSystem with
            member _.CreateLot msg = Workflow.createLot store msg   
            member _.UpdateLot msg = Workflow.updateLot store conn msg
            member _.DeleteLot msg = Workflow.deleteLot store msg
            member _.GetLot msg = async {
                match! Query.getLot conn msg.Payload with
                | Some lot when msg.CurrentUser.HasAccessToBuilding lot.BuildingId ->
                    return Some lot
                | _ ->
                    return None
            }
            member _.GetLots msg = async {
                if msg.CurrentUser.HasAccessToBuilding msg.Payload
                then
                    return! Query.getLots conn msg.Payload
                else
                    return []
            }
    }