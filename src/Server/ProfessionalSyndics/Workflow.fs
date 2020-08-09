module Server.ProfessionalSyndics.Workflow

open System
open Shared.Remoting
open Shared.Read
open Shared.Write
open Server.Library
open Server.LibraryExtensions
open Storage

let createProfessionalSyndic (storage: IProfessionalSyndicStorage) (msg: Message<ProfessionalSyndic>): Async<Result<unit, CreateProfessionalSyndicError>> = async {
    if msg.CurrentUser.IsSysAdmin
    then
        let validated = ValidatedProfessionalSyndic.Validate (msg.Payload)
        match validated with
        | Ok validated ->
            do! storage.CreateProfessionalSyndic validated
            return Ok ()
        | Error validationErrors ->
            return Error (CreateProfessionalSyndicError.Validation validationErrors)
    else
        return Error CreateProfessionalSyndicError.AuthorizationError
}

let updateProfessionalSyndic (storage: IProfessionalSyndicStorage) (msg: Message<ProfessionalSyndic>): Async<Result<unit, UpdateProfessionalSyndicError>> = async {
    if msg.CurrentUser.IsSysAdmin
    then
        let validated = ValidatedProfessionalSyndic.Validate (msg.Payload)
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.UpdateProfessionalSyndic validated
            if nbRowsAffected = 0
            then return Error (UpdateProfessionalSyndicError.NotFound)
            else return Ok ()
        | Error validationErrors ->
            return Error (UpdateProfessionalSyndicError.Validation validationErrors)
    else
        return Error UpdateProfessionalSyndicError.AuthorizationError
}

let deleteProfessionalSyndic (storage: IProfessionalSyndicStorage) (msg: Message<Guid>): Async<Result<unit, DeleteProfessionalSyndicError>> = async {
    if msg.CurrentUser.IsSysAdmin
    then
        let! nbRowsAffected = storage.DeleteProfessionalSyndic (msg.Payload)
        if nbRowsAffected = 0
        then return Error DeleteProfessionalSyndicError.NotFound
        else return Ok ()
    else
        return Error DeleteProfessionalSyndicError.AuthorizationError
}