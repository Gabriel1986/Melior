module Server.ProfessionalSyndics.Workflow

open System
open Shared.Remoting
open Shared.Read
open Shared.Write
open Server.Library
open Server.LibraryExtensions
open Server.Blueprint.Behavior.Storage
open Server.Blueprint.Data.Storage

let createProfessionalSyndic (storage: IStorageEngine) (msg: Message<ProfessionalSyndic>): Async<Result<unit, SaveProfessionalSyndicError>> = async {
    //Only a systems admin can create a professional syndic
    if msg.CurrentUser.IsSysAdmin ()
    then
        let validated = ValidatedProfessionalSyndic.Validate msg.Payload
        match validated with
        | Ok validated ->
            let! _ = storage.PersistTransactional [
                validated
                |> CUDEvent.Created
                |> ProfessionalSyndicEvent.ProfessionalSyndicEvent
                |> StorageEvent.ProfessionalSyndicEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveProfessionalSyndicError.Validation validationErrors)
    else
        return Error SaveProfessionalSyndicError.AuthorizationError
}

let updateProfessionalSyndic (storage: IStorageEngine) (msg: Message<ProfessionalSyndic>): Async<Result<unit, SaveProfessionalSyndicError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding(msg.Payload.Organization.OrganizationId)
    then
        let validated = ValidatedProfessionalSyndic.Validate msg.Payload
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.PersistTransactional [
                validated
                |> CUDEvent.Updated
                |> ProfessionalSyndicEvent.ProfessionalSyndicEvent
                |> StorageEvent.ProfessionalSyndicEvent
                |> inMsg msg
            ]
            if nbRowsAffected = 0
            then return Error (SaveProfessionalSyndicError.NotFound)
            else return Ok ()
        | Error validationErrors ->
            return Error (SaveProfessionalSyndicError.Validation validationErrors)
    else
        return Error SaveProfessionalSyndicError.AuthorizationError
}

let deleteProfessionalSyndic (storage: IStorageEngine) (msg: Message<Guid>): Async<Result<unit, DeleteProfessionalSyndicError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding msg.Payload
    then
        let! nbRowsAffected = storage.PersistTransactional [
            msg.Payload
            |> CUDEvent.Deleted
            |> ProfessionalSyndicEvent.ProfessionalSyndicEvent
            |> StorageEvent.ProfessionalSyndicEvent
            |> inMsg msg
        ]
        if nbRowsAffected = 0
        then return Error DeleteProfessionalSyndicError.NotFound
        else return Ok ()
    else
        return Error DeleteProfessionalSyndicError.AuthorizationError
}