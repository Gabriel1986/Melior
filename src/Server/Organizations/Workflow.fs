module Server.Organizations.Workflow

open System
open Server.Library
open Shared.Write

let createContactPerson (connectionString: string) (msg: Message<ValidatedContactPerson>) = async {
    do! Server.Organizations.Storage.createContactPerson connectionString msg.Payload
    return Ok ()
}

let updateContactPerson (connectionString: string) (msg: Message<ValidatedContactPerson>) = async {
    do! Server.Organizations.Storage.updateContactPerson connectionString msg.Payload
    return Ok ()
}

let deleteContactPerson (connectionString: string) (msg: Message<Guid>) = async {
    do! Server.Organizations.Storage.deleteContactPerson connectionString msg.Payload
    return Ok ()
}

let createOrganization (connectionString: string) (msg: Message<ValidatedOrganization>) = async {
    do! Server.Organizations.Storage.createOrganization connectionString msg.Payload
    return Ok ()
}

let updateOrganization (connectionString: string) (msg: Message<ValidatedOrganization>) = async {
    do! Server.Organizations.Storage.updateOrganization connectionString msg.Payload
    return Ok ()
}

let deleteOrganization (connectionString: string) (msg: Message<Guid>) = async {
    do! Server.Organizations.Storage.deleteOrganization connectionString msg.Payload
    return Ok ()
}

let createOrganizationType (connectionString: string) (msg: Message<ValidatedOrganizationType>) = async {
    do! Server.Organizations.Storage.createOrganizationType connectionString msg.Payload
    return Ok ()
}

let updateOrganizationType (connectionString: string) (msg: Message<ValidatedOrganizationType>) = async {
    do! Server.Organizations.Storage.updateOrganizationType connectionString msg.Payload
    return Ok ()
}

let deleteOrganizationType (connectionString: string) (msg: Message<Guid>) = async {
    do! Server.Organizations.Storage.deleteOrganizationType connectionString msg.Payload
    return Ok ()
}