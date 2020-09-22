module Server.Lots.Workflow

open System
open Shared.Remoting
open Shared.Read
open Shared.Write
open Server.Library
open Server.LibraryExtensions
open Storage

let createLot (storage: ILotStorage) (msg: Message<Lot>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId)
    then
        let validated = ValidatedLot.Validate (msg.Payload)
        match validated with
        | Ok validated ->
            do! storage.CreateLot validated
            return Ok ()
        | Error validationErrors ->
            return Error (SaveLotError.Validation validationErrors)
    else
        return Error SaveLotError.AuthorizationError
}

let updateLot (storage: ILotStorage) (msg: Message<Lot>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId)
    then
        let validated = ValidatedLot.Validate (msg.Payload)
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.UpdateLot validated
            if nbRowsAffected = 0
            then return Error (SaveLotError.NotFound)
            else return Ok ()
        | Error validationErrors ->
            return Error (SaveLotError.Validation validationErrors)
    else
        return Error SaveLotError.AuthorizationError
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