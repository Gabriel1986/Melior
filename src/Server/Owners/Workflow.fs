module Server.Owners.Workflow

open System
open Shared.Remoting
open Shared.Read
open Shared.Write
open Server.Library
open Server.LibraryExtensions
open Storage

let createOwner (storage: IOwnerStorage) (msg: Message<Owner>): Async<Result<unit, CreateOwnerError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId)
    then
        let validated = ValidatedOwner.Validate (msg.Payload)
        match validated with
        | Ok validated ->
            do! storage.CreateOwner validated
            return Ok ()
        | Error validationErrors ->
            return Error (CreateOwnerError.Validation validationErrors)
    else
        return Error CreateOwnerError.AuthorizationError
}

let updateOwner (storage: IOwnerStorage) (msg: Message<Owner>): Async<Result<unit, UpdateOwnerError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId)
    then
        let validated = ValidatedOwner.Validate (msg.Payload)
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.UpdateOwner validated
            if nbRowsAffected = 0
            then return Error (UpdateOwnerError.NotFound)
            else return Ok ()
        | Error validationErrors ->
            return Error (UpdateOwnerError.Validation validationErrors)
    else
        return Error UpdateOwnerError.AuthorizationError
}

let deleteOwner (storage: IOwnerStorage) (msg: Message<BuildingId * Guid>): Async<Result<unit, DeleteOwnerError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload)
    then
        let! nbRowsAffected = storage.DeleteOwner (msg.Payload)
        if nbRowsAffected = 0
        then return Error DeleteOwnerError.NotFound
        else return Ok ()
    else
        return Error DeleteOwnerError.AuthorizationError
}