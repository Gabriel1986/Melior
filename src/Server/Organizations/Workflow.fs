module Server.Organizations.Workflow

open System
open Shared.Read
open Shared.Write
open Shared.Remoting
open Server.Library
open Server.LibraryExtensions
open Storage

let createContactPerson (storage: IOrganizationStorage) (msg: Message<ContactPerson>): Async<Result<unit, CreateContactPersonError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId |> Option.defaultValue Guid.Empty)
    then
        let validated = ValidatedContactPerson.Validate (msg.Payload)
        match validated with
        | Ok validated ->
            do! storage.CreateContactPerson validated
            return Ok ()
        | Error validationErrors ->
            return Error (CreateContactPersonError.Validation validationErrors)
    else
        return Error CreateContactPersonError.AuthorizationError
}

let updateContactPerson (storage: IOrganizationStorage) (msg: Message<ContactPerson>): Async<Result<unit, UpdateContactPersonError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId |> Option.defaultValue Guid.Empty)
    then
        let validated = ValidatedContactPerson.Validate (msg.Payload)
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.UpdateContactPerson validated
            if nbRowsAffected = 0
            then return Error UpdateContactPersonError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (UpdateContactPersonError.Validation validationErrors)
    else
        return Error UpdateContactPersonError.AuthorizationError
}

let deleteContactPerson (storage: IOrganizationStorage) (msg: Message<BuildingId option * Guid>): Async<Result<unit, DeleteContactPersonError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload |> Option.defaultValue Guid.Empty)
    then
        let! nbRowsAffected = storage.DeleteContactPerson (msg.Payload)
        if nbRowsAffected = 0
        then return Error DeleteContactPersonError.NotFound
        else return Ok ()
    else
        return Error DeleteContactPersonError.AuthorizationError
}

let createOrganization (storage: IOrganizationStorage) (msg: Message<Organization>): Async<Result<unit, CreateOrganizationError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId |> Option.defaultValue Guid.Empty)
    then
        let validated = ValidatedOrganization.Validate (msg.Payload)
        match validated with
        | Ok validated ->
            do! storage.CreateOrganization validated
            return Ok ()
        | Error validationErrors ->
            return Error (CreateOrganizationError.Validation validationErrors)
    else
        return Error CreateOrganizationError.AuthorizationError
}

let updateOrganization (storage: IOrganizationStorage) (msg: Message<Organization>): Async<Result<unit, UpdateOrganizationError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId |> Option.defaultValue Guid.Empty)
    then
        let validated = ValidatedOrganization.Validate (msg.Payload)
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.UpdateOrganization validated
            if nbRowsAffected = 0
            then return Error UpdateOrganizationError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (UpdateOrganizationError.Validation validationErrors)
    else
        return Error UpdateOrganizationError.AuthorizationError
}

let deleteOrganization (storage: IOrganizationStorage) (msg: Message<BuildingId option * Guid>): Async<Result<unit, DeleteOrganizationError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload |> Option.defaultValue Guid.Empty)
    then
        let! nbRowsAffected = storage.DeleteOrganization (msg.Payload)
        if nbRowsAffected = 0
        then return Error DeleteOrganizationError.NotFound
        else return Ok ()
    else
        return Error DeleteOrganizationError.AuthorizationError
}

let createOrganizationType (storage: IOrganizationStorage) (msg: Message<OrganizationType>): Async<Result<unit, CreateOrganizationTypeError>> = async {
    if msg.CurrentUser.IsSysAdmin ()
    then
        let validated = ValidatedOrganizationType.Validate (msg.Payload)
        match validated with
        | Ok validated ->
            do! storage.CreateOrganizationType validated
            return Ok ()
        | Error validationErrors ->
            return Error (CreateOrganizationTypeError.Validation validationErrors)
    else
        return Error CreateOrganizationTypeError.AuthorizationError
}

let updateOrganizationType (storage: IOrganizationStorage) (msg: Message<OrganizationType>): Async<Result<unit, UpdateOrganizationTypeError>> = async {
    if msg.CurrentUser.IsSysAdmin ()
    then
        let validated = ValidatedOrganizationType.Validate (msg.Payload)
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.UpdateOrganizationType validated
            if nbRowsAffected = 0
            then return Error UpdateOrganizationTypeError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (UpdateOrganizationTypeError.Validation validationErrors)
    else
        return Error UpdateOrganizationTypeError.AuthorizationError
}

let deleteOrganizationType (storage: IOrganizationStorage) (msg: Message<Guid>): Async<Result<unit, DeleteOrganizationTypeError>> = async {
    if msg.CurrentUser.IsSysAdmin ()
    then
        let! nbRowsAffected = storage.DeleteOrganizationType (msg.Payload)
        if nbRowsAffected = 0
        then return Error DeleteOrganizationTypeError.NotFound
        else return Ok ()
    else
        return Error DeleteOrganizationTypeError.AuthorizationError
}