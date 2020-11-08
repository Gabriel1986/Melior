module Server.Organizations.Workflow

open System
open Shared.Read
open Shared.Write
open Shared.Remoting
open Server.Library
open Server.LibraryExtensions
open Storage

let createContactPerson (storage: IOrganizationStorage) (msg: Message<ContactPerson>): Async<Result<unit, SaveContactPersonError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId |> Option.defaultValue Guid.Empty)
    then
        let validated = ValidatedContactPerson.Validate msg.Payload
        match validated with
        | Ok validated ->
            do! storage.CreateContactPerson validated
            return Ok ()
        | Error validationErrors ->
            return Error (SaveContactPersonError.Validation validationErrors)
    else
        return Error SaveContactPersonError.AuthorizationError
}

let updateContactPerson (storage: IOrganizationStorage) (msg: Message<ContactPerson>): Async<Result<unit, SaveContactPersonError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId |> Option.defaultValue Guid.Empty)
    then
        let validated = ValidatedContactPerson.Validate msg.Payload
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.UpdateContactPerson validated
            if nbRowsAffected = 0
            then return Error SaveContactPersonError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (SaveContactPersonError.Validation validationErrors)
    else
        return Error SaveContactPersonError.AuthorizationError
}

let deleteContactPerson (storage: IOrganizationStorage) (msg: Message<BuildingId option * Guid>): Async<Result<unit, DeleteContactPersonError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload |> Option.defaultValue Guid.Empty)
    then
        let! nbRowsAffected = storage.DeleteContactPerson msg.Payload
        if nbRowsAffected = 0
        then return Error DeleteContactPersonError.NotFound
        else return Ok ()
    else
        return Error DeleteContactPersonError.AuthorizationError
}

let createOrganization (storage: IOrganizationStorage) (msg: Message<Organization>): Async<Result<unit, SaveOrganizationError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId |> Option.defaultValue Guid.Empty)
    then
        let validated = ValidatedOrganization.Validate msg.Payload
        match validated with
        | Ok validated ->
            do! storage.CreateOrganization validated
            return Ok ()
        | Error validationErrors ->
            return Error (SaveOrganizationError.Validation validationErrors)
    else
        return Error SaveOrganizationError.AuthorizationError
}

let updateOrganization (storage: IOrganizationStorage) (msg: Message<Organization>): Async<Result<unit, SaveOrganizationError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId |> Option.defaultValue Guid.Empty)
    then
        let validated = ValidatedOrganization.Validate msg.Payload
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.UpdateOrganization validated
            if nbRowsAffected = 0
            then return Error SaveOrganizationError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (SaveOrganizationError.Validation validationErrors)
    else
        return Error SaveOrganizationError.AuthorizationError
}

let deleteOrganization (storage: IOrganizationStorage) (msg: Message<BuildingId option * Guid>): Async<Result<unit, DeleteOrganizationError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload |> Option.defaultValue Guid.Empty)
    then
        let! nbRowsAffected = storage.DeleteOrganization msg.Payload
        if nbRowsAffected = 0
        then return Error DeleteOrganizationError.NotFound
        else return Ok ()
    else
        return Error DeleteOrganizationError.AuthorizationError
}

let createOrganizationType (storage: IOrganizationStorage) (msg: Message<OrganizationType>): Async<Result<unit, SaveOrganizationTypeError>> = async {
    if msg.CurrentUser.IsSysAdmin ()
    then
        let validated = ValidatedOrganizationType.Validate msg.Payload
        match validated with
        | Ok validated ->
            do! storage.CreateOrganizationType validated
            return Ok ()
        | Error validationErrors ->
            return Error (SaveOrganizationTypeError.Validation validationErrors)
    else
        return Error SaveOrganizationTypeError.AuthorizationError
}

let updateOrganizationType (storage: IOrganizationStorage) (msg: Message<OrganizationType>): Async<Result<unit, SaveOrganizationTypeError>> = async {
    if msg.CurrentUser.IsSysAdmin ()
    then
        let validated = ValidatedOrganizationType.Validate msg.Payload
        match validated with
        | Ok validated -> 
            let! nbRowsAffected = storage.UpdateOrganizationType validated
            if nbRowsAffected = 0
            then return Error SaveOrganizationTypeError.NotFound
            else return Ok ()
        | Error validationErrors ->
            return Error (SaveOrganizationTypeError.Validation validationErrors)
    else
        return Error SaveOrganizationTypeError.AuthorizationError
}

let deleteOrganizationType (storage: IOrganizationStorage) (msg: Message<Guid>): Async<Result<unit, DeleteOrganizationTypeError>> = async {
    if msg.CurrentUser.IsSysAdmin ()
    then
        let! nbRowsAffected = storage.DeleteOrganizationType msg.Payload
        if nbRowsAffected = 0
        then return Error DeleteOrganizationTypeError.NotFound
        else return Ok ()
    else
        return Error DeleteOrganizationTypeError.AuthorizationError
}