module Server.Owners.Workflow

open System
open Server.Blueprint.Behavior.Storage
open Server.Blueprint.Data.Storage
open Shared.Remoting
open Shared.Read
open Shared.Write
open Shared.ConstrainedTypes
open Server.Library
open Server.LibraryExtensions

let createOwner (storage: IStorageEngine) (msg: Message<Owner>): Async<Result<unit, SaveOwnerError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId)
    then
        let validated = ValidatedOwner.Validate msg.Payload
        match validated with
        | Ok validated ->
            let! _ = storage.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Created
                |> OwnerEvent.OwnerEvent
                |> StorageEvent.OwnerEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveOwnerError.Validation validationErrors)
    else
        return Error SaveOwnerError.AuthorizationError
}

let updateOwner (storage: IStorageEngine) (msg: Message<Owner>): Async<Result<unit, SaveOwnerError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId)
    then
        let validated = ValidatedOwner.Validate msg.Payload
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Updated
                |> OwnerEvent.OwnerEvent
                |> StorageEvent.OwnerEvent
                |> inMsg msg
            ]
            if nbRowsAffected = 0
            then return Error (SaveOwnerError.NotFound)
            else return Ok ()
        | Error validationErrors ->
            return Error (SaveOwnerError.Validation validationErrors)
    else
        return Error SaveOwnerError.AuthorizationError
}

let deleteOwner (storage: IStorageEngine) (msg: Message<BuildingId * Guid>): Async<Result<unit, DeleteOwnerError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload)
    then
        let! nbRowsAffected = storage.PersistTransactional [
            msg.Payload
            |> BuildingSpecificCUDEvent.Deleted
            |> OwnerEvent.OwnerEvent
            |> StorageEvent.OwnerEvent
            |> inMsg msg
        ]
        if nbRowsAffected = 0
        then return Error DeleteOwnerError.NotFound
        else return Ok ()
    else
        return Error DeleteOwnerError.AuthorizationError
}