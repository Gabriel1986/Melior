module Server.Lots.Workflow

open System
open Shared.Remoting
open Shared.Read
open Shared.Write
open Server.Library
open Server.LibraryExtensions
open Storage

let createLot (storage: ILotStorage) (msg: Message<Lot>): Async<Result<unit, CreateLotError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId)
    then
        let validated = ValidatedLot.Validate (msg.Payload)
        match validated with
        | Ok validated ->
            do! storage.CreateLot validated
            return Ok ()
        | Error validationErrors ->
            return Error (CreateLotError.Validation validationErrors)
    else
        return Error CreateLotError.AuthorizationError
}

let updateLot (storage: ILotStorage) (msg: Message<Lot>): Async<Result<unit, UpdateLotError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId)
    then
        let validated = ValidatedLot.Validate (msg.Payload)
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.UpdateLot validated
            if nbRowsAffected = 0
            then return Error (UpdateLotError.NotFound)
            else return Ok ()
        | Error validationErrors ->
            return Error (UpdateLotError.Validation validationErrors)
    else
        return Error UpdateLotError.AuthorizationError
}

let deleteLot (storage: ILotStorage) (msg: Message<BuildingId * Guid>): Async<Result<unit, DeleteLotError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload)
    then
        let! nbRowsAffected = storage.DeleteLot (msg.Payload)
        if nbRowsAffected = 0
        then return Error DeleteLotError.NotFound
        else return Ok ()
    else
        return Error DeleteLotError.AuthorizationError
}