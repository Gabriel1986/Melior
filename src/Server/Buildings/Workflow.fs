module Server.Buildings.Workflow

open System
open Shared.Remoting
open Shared.Write
open Shared.Read
open Server.Library
open Server.LibraryExtensions
open Server.Blueprint.Behavior.Storage
open Server.Blueprint.Data.Storage

//TODO: license check if we ever sell to professional syndics?
let createBuilding (storageEngine: IStorageEngine) (msg: Message<Building>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding msg.Payload.BuildingId
    then
        let validationResult = ValidatedBuilding.Validate msg.Payload
        match validationResult with
        | Ok validated ->
            let! _ = storageEngine.PersistTransactional [
                CUDEvent.Created validated
                |> BuildingEvent.BuildingEvent
                |> StorageEvent.BuildingEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveBuildingError.Validation validationErrors)
    else
        return Error SaveBuildingError.AuthorizationError
}

let updateBuilding (storageEngine: IStorageEngine) (msg: Message<Building>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding msg.Payload.BuildingId
    then
        let validationResult = ValidatedBuilding.Validate msg.Payload
        match validationResult with
        | Ok validated ->
            let! nbRowsAffected = storageEngine.PersistTransactional [
                CUDEvent.Updated validated
                |> BuildingEvent.BuildingEvent
                |> StorageEvent.BuildingEvent
                |> inMsg msg
            ]
            if nbRowsAffected = 0
            then return Error SaveBuildingError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (SaveBuildingError.Validation validationErrors)
    else
        return Error SaveBuildingError.AuthorizationError
}

let deleteBuilding (storageEngine: IStorageEngine) (msg: Message<Guid>): Async<Result<unit, DeleteBuildingError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding msg.Payload
    then
        let! nbRowsAffected = storageEngine.PersistTransactional [
            CUDEvent.Deleted msg.Payload
            |> BuildingEvent.BuildingEvent
            |> StorageEvent.BuildingEvent
            |> inMsg msg
        ]
        if nbRowsAffected = 0
        then return Error DeleteBuildingError.NotFound
        else return Ok ()
    else
        return Error DeleteBuildingError.AuthorizationError
}


let updateBuildingSyndic (storageEngine: IStorageEngine) (msg: Message<BuildingId * SyndicInput option>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload)
    then
        let validationResult = 
            match snd msg.Payload with
            | Some input -> ValidatedSyndic.Validate input |> Result.map Some
            | None -> Ok None
        match validationResult with
        | Ok validated ->
            let! nbRowsAffected = storageEngine.PersistTransactional [                
                BuildingEvent.SyndicWasUpdated (fst msg.Payload, validated)
                |> StorageEvent.BuildingEvent
                |> inMsg msg
            ]
            if nbRowsAffected = 0
            then return Error SaveBuildingError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (SaveBuildingError.Validation validationErrors)
    else
        return Error SaveBuildingError.AuthorizationError
}

let updateBuildingConcierge (storageEngine: IStorageEngine) (msg: Message<BuildingId * ConciergeInput option>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload)
    then
        let validationResult = 
            match snd msg.Payload with
            | Some input -> ValidatedConcierge.Validate input |> Result.map Some
            | None -> Ok None
        match validationResult with
        | Ok validated ->
            let! nbRowsAffected = storageEngine.PersistTransactional [
                BuildingEvent.ConciergeWasUpdated (fst msg.Payload, validated)
                |> StorageEvent.BuildingEvent
                |> inMsg msg
            ]
            if nbRowsAffected = 0
            then return Error SaveBuildingError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (SaveBuildingError.Validation validationErrors)
    else
        return Error SaveBuildingError.AuthorizationError
}