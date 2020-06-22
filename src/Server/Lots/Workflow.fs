module Server.Lots.Workflow

open System
open Server.Library
open Shared.Remoting
open Shared.Write

let createLot connectionString (message: Message<ValidatedLot>) = async {
    do! Server.Lots.Storage.createLot connectionString message.Payload
    return Ok ()
}

let updateLot connectionString (message: Message<ValidatedLot>) = async {
    do! Server.Lots.Storage.updateLot connectionString message.Payload
    return Ok ()
}

let deleteLot connectionString (message: Message<Guid>): Async<Result<unit, AuthorizationError>> = async {
    do! Server.Lots.Storage.deleteLot connectionString message.Payload
    return Ok ()
}
