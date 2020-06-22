module Server.Owners.Workflow

open System
open Shared.Remoting
open Shared.Write
open Server.Library

let createOwner connectionString (message: Message<ValidatedOwner>) = async {
    do! Server.Owners.Storage.createOwner connectionString message.Payload
    return Ok()
}

let updateOwner connectionString (message: Message<ValidatedOwner>) = async {
    do! Server.Owners.Storage.updateOwner connectionString message.Payload
    return Ok()
}

let deleteOwner connectionString (message: Message<Guid>): Async<Result<unit, AuthorizationError>> = async {
    do! Server.Owners.Storage.deleteOwner connectionString message.Payload
    return Ok ()
}