module Server.Organizations.Workflow

open System
open Shared.Read
open Shared.Write
open Shared.Remoting
open Server.Blueprint.Behavior.Storage
open Server.Blueprint.Data.Storage
open Server.Library
open Server.LibraryExtensions

let createContactPerson (storage: IStorageEngine) (msg: Message<ContactPerson>): Async<Result<unit, SaveContactPersonError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId |> Option.defaultValue Guid.Empty)
    then
        let validated = ValidatedContactPerson.Validate msg.Payload
        match validated with
        | Ok validated ->
            let! _ = storage.PersistTransactional [
                validated
                |> OrganizationEvent.ContactPersonWasCreated
                |> StorageEvent.OrganizationEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveContactPersonError.Validation validationErrors)
    else
        return Error SaveContactPersonError.AuthorizationError
}

let updateContactPerson (storage: IStorageEngine) (msg: Message<ContactPerson>): Async<Result<unit, SaveContactPersonError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId |> Option.defaultValue Guid.Empty)
    then
        let validated = ValidatedContactPerson.Validate msg.Payload
        match validated with
        | Ok validated ->            
            let! nbRowsAffected = storage.PersistTransactional [
                validated
                |> OrganizationEvent.ContactPersonWasUpdated
                |> StorageEvent.OrganizationEvent
                |> inMsg msg
            ]
            if nbRowsAffected = 0
            then return Error SaveContactPersonError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (SaveContactPersonError.Validation validationErrors)
    else
        return Error SaveContactPersonError.AuthorizationError
}

let deleteContactPerson (storage: IStorageEngine) (msg: Message<BuildingId option * Guid>): Async<Result<unit, DeleteContactPersonError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload |> Option.defaultValue Guid.Empty)
    then
        let! nbRowsAffected = storage.PersistTransactional [
            msg.Payload
            |> OrganizationEvent.ContactPersonWasDeleted
            |> StorageEvent.OrganizationEvent
            |> inMsg msg
        ]
        if nbRowsAffected = 0
        then return Error DeleteContactPersonError.NotFound
        else return Ok ()
    else
        return Error DeleteContactPersonError.AuthorizationError
}

let createOrganization (storage: IStorageEngine) (msg: Message<Organization>): Async<Result<unit, SaveOrganizationError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId |> Option.defaultValue Guid.Empty)
    then
        let validated = ValidatedOrganization.Validate msg.Payload
        match validated with
        | Ok validated ->
            let! _ = storage.PersistTransactional [
                validated
                |> OrganizationEvent.OrganizationWasCreated
                |> StorageEvent.OrganizationEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveOrganizationError.Validation validationErrors)
    else
        return Error SaveOrganizationError.AuthorizationError
}

let updateOrganization (storage: IStorageEngine) (msg: Message<Organization>): Async<Result<unit, SaveOrganizationError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId |> Option.defaultValue Guid.Empty)
    then
        let validated = ValidatedOrganization.Validate msg.Payload
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.PersistTransactional [
                validated
                |> OrganizationEvent.OrganizationWasUpdated
                |> StorageEvent.OrganizationEvent
                |> inMsg msg
            ]
            if nbRowsAffected = 0
            then return Error SaveOrganizationError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (SaveOrganizationError.Validation validationErrors)
    else
        return Error SaveOrganizationError.AuthorizationError
}

let deleteOrganization (storage: IStorageEngine) (msg: Message<BuildingId option * Guid>): Async<Result<unit, DeleteOrganizationError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload |> Option.defaultValue Guid.Empty)
    then
        let! nbRowsAffected = storage.PersistTransactional [
            msg.Payload
            |> OrganizationEvent.OrganizationWasDeleted
            |> StorageEvent.OrganizationEvent
            |> inMsg msg
        ]
        if nbRowsAffected = 0
        then return Error DeleteOrganizationError.NotFound
        else return Ok ()
    else
        return Error DeleteOrganizationError.AuthorizationError
}

let createOrganizationType (storage: IStorageEngine) (msg: Message<OrganizationType>): Async<Result<unit, SaveOrganizationTypeError>> = async {
    if msg.CurrentUser.IsSysAdmin ()
    then
        let validated = ValidatedOrganizationType.Validate msg.Payload
        match validated with
        | Ok validated ->
            let! _ = storage.PersistTransactional [
                validated
                |> CUDEvent.Created
                |> OrganizationEvent.OrganizationTypeEvent
                |> StorageEvent.OrganizationEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveOrganizationTypeError.Validation validationErrors)
    else
        return Error SaveOrganizationTypeError.AuthorizationError
}

let updateOrganizationType (storage: IStorageEngine) (msg: Message<OrganizationType>): Async<Result<unit, SaveOrganizationTypeError>> = async {
    if msg.CurrentUser.IsSysAdmin ()
    then
        let validated = ValidatedOrganizationType.Validate msg.Payload
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.PersistTransactional [
                validated
                |> CUDEvent.Updated
                |> OrganizationEvent.OrganizationTypeEvent
                |> StorageEvent.OrganizationEvent
                |> inMsg msg
            ]
            if nbRowsAffected = 0
            then return Error SaveOrganizationTypeError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (SaveOrganizationTypeError.Validation validationErrors)
    else
        return Error SaveOrganizationTypeError.AuthorizationError
}

let deleteOrganizationType (storage: IStorageEngine) (msg: Message<Guid>): Async<Result<unit, DeleteOrganizationTypeError>> = async {
    if msg.CurrentUser.IsSysAdmin ()
    then
        let! nbRowsAffected = storage.PersistTransactional [
            msg.Payload
            |> CUDEvent.Deleted
            |> OrganizationEvent.OrganizationTypeEvent
            |> StorageEvent.OrganizationEvent
            |> inMsg msg
        ]
        if nbRowsAffected = 0
        then return Error DeleteOrganizationTypeError.NotFound
        else return Ok ()
    else
        return Error DeleteOrganizationTypeError.AuthorizationError
}