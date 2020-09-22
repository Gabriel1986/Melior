﻿module Server.Financial.Workflow

open System
open Storage
open Server.Library
open Server.LibraryExtensions
open Shared.Read
open Shared.Write
open Shared.Remoting

let (|Authorized|Unauthorized|) (currentUser: User, buildingId: BuildingId option) =
    match buildingId with
    | Some buildingId when currentUser.HasAccessToBuilding buildingId -> Authorized
    | None when currentUser.IsSysAdmin() -> Authorized
    | _ -> Unauthorized

let createDistributionKey (store: IFinancialStorage) (msg: Message<DistributionKey>) = async {
    match (msg.CurrentUser, msg.Payload.BuildingId) with
    | Authorized ->
        let validated = ValidatedDistributionKey.Validate (msg.Payload)
        match validated with
        | Ok validated -> 
            do! store.CreateDistributionKey (msg |> Message.map validated)
            return Ok ()
        | Error validationErrors ->
            return Error (SaveDistributionKeyError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveDistributionKeyError.AuthorizationError
}

let updateDistributionKey (store: IFinancialStorage) (msg: Message<DistributionKey>) = async {
    match (msg.CurrentUser, msg.Payload.BuildingId) with
    | Authorized ->
        let validated = ValidatedDistributionKey.Validate (msg.Payload)
        match validated with
        | Ok validated ->
            let! nbUpdated = store.UpdateDistributionKey (msg |> Message.map validated)
            return if nbUpdated > 0 then Ok () else Error (SaveDistributionKeyError.NotFound)
        | Error validationErrors ->
            return Error (SaveDistributionKeyError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveDistributionKeyError.AuthorizationError
}

let deleteDistributionKey (store: IFinancialStorage) (msg: Message<BuildingId * Guid>) = async {
    match (msg.CurrentUser, Some (fst msg.Payload)) with
    | Authorized ->
        let! nbRows = store.DeleteDistributionKey msg
        return if nbRows > 0 then Ok () else Error DeleteDistributionKeyError.NotFound
    | Unauthorized ->
        return Error DeleteDistributionKeyError.AuthorizationError
}