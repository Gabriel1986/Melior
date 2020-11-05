module Server.ProfessionalSyndics.Workflow

open System
open Shared.Remoting
open Shared.Read
open Shared.Write
open Server.Library
open Server.LibraryExtensions
open Server.IbanValidator
open Storage

let createProfessionalSyndic (storage: IProfessionalSyndicStorage) (msg: Message<ProfessionalSyndic>): Async<Result<unit, SaveProfessionalSyndicError>> = async {
    //Only a systems admin can create a professional syndic
    if msg.CurrentUser.IsSysAdmin ()
    then
        let validated = ValidatedProfessionalSyndic.Validate validateIban msg.Payload
        match validated with
        | Ok validated ->
            do! storage.CreateProfessionalSyndic validated
            return Ok ()
        | Error validationErrors ->
            return Error (SaveProfessionalSyndicError.Validation validationErrors)
    else
        return Error SaveProfessionalSyndicError.AuthorizationError
}

let updateProfessionalSyndic (storage: IProfessionalSyndicStorage) (msg: Message<ProfessionalSyndic>): Async<Result<unit, SaveProfessionalSyndicError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding(msg.Payload.Organization.OrganizationId)
    then
        let validated = ValidatedProfessionalSyndic.Validate validateIban msg.Payload
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.UpdateProfessionalSyndic validated
            if nbRowsAffected = 0
            then return Error (SaveProfessionalSyndicError.NotFound)
            else return Ok ()
        | Error validationErrors ->
            return Error (SaveProfessionalSyndicError.Validation validationErrors)
    else
        return Error SaveProfessionalSyndicError.AuthorizationError
}

let deleteProfessionalSyndic (storage: IProfessionalSyndicStorage) (msg: Message<Guid>): Async<Result<unit, DeleteProfessionalSyndicError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding msg.Payload
    then
        let! nbRowsAffected = storage.DeleteProfessionalSyndic msg.Payload
        if nbRowsAffected = 0
        then return Error DeleteProfessionalSyndicError.NotFound
        else return Ok ()
    else
        return Error DeleteProfessionalSyndicError.AuthorizationError
}