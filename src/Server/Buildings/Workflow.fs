module Server.Buildings.Workflow

open System
open Shared.Remoting
open Shared.Write
open Shared.Read
open Server.Library
open Server.LibraryExtensions
open Storage

//TODO: license check if we ever sell to professional syndics?
let createBuilding (storage: IBuildingStorage) (msg: Message<Building>): Async<Result<unit, CreateBuildingError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding msg.Payload.BuildingId
    then
        let validated = ValidatedBuilding.Validate msg.Payload
        match validated with
        | Ok validated ->
            do! storage.CreateBuilding validated
            return Ok ()
        | Error validationErrors ->
            return Error (CreateBuildingError.Validation validationErrors)
    else
        return Error CreateBuildingError.AuthorizationError
}

let updateBuilding (storage: IBuildingStorage) (msg: Message<Building>): Async<Result<unit, UpdateBuildingError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding msg.Payload.BuildingId
    then
        let validated = ValidatedBuilding.Validate msg.Payload
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.UpdateBuilding validated
            if nbRowsAffected = 0
            then return Error UpdateBuildingError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (UpdateBuildingError.Validation validationErrors)
    else
        return Error UpdateBuildingError.AuthorizationError
}

let deleteBuilding (storage: IBuildingStorage) (msg: Message<Guid>): Async<Result<unit, DeleteBuildingError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding msg.Payload
    then
        let! nbRowsAffected = storage.DeleteBuilding msg.Payload
        if nbRowsAffected = 0
        then return Error DeleteBuildingError.NotFound
        else return Ok ()
    else
        return Error DeleteBuildingError.AuthorizationError
}


let updateBuildingSyndic (storage: IBuildingStorage) (msg: Message<BuildingId * SyndicInput option>): Async<Result<unit, UpdateBuildingError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload)
    then
        let validated = 
            match snd msg.Payload with
            | Some input -> ValidatedSyndicInput.Validate input |> Result.map Some
            | None -> Ok None
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.UpdateBuildingSyndic (fst msg.Payload, validated)
            if nbRowsAffected = 0
            then return Error UpdateBuildingError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (UpdateBuildingError.Validation validationErrors)
    else
        return Error UpdateBuildingError.AuthorizationError
}

let updateBuildingConcierge (storage: IBuildingStorage) (msg: Message<BuildingId * ConciergeInput option>): Async<Result<unit, UpdateBuildingError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload)
    then
        let validated = 
            match snd msg.Payload with
            | Some input -> ValidatedConciergeInput.Validate input |> Result.map Some
            | None -> Ok None
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.UpdateBuildingConcierge (fst msg.Payload, validated)
            if nbRowsAffected = 0
            then return Error UpdateBuildingError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (UpdateBuildingError.Validation validationErrors)
    else
        return Error UpdateBuildingError.AuthorizationError
}