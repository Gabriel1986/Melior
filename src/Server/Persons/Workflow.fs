module Server.Persons.Workflow

open Server.Library
open Shared.Remoting
open Shared.Write

let createPerson (connectionString: string) (msg: Message<ValidatedPerson>): Async<Result<unit, CreatePersonError>> = async {
    do! Server.Persons.Storage.createPerson connectionString msg.Payload
    return Ok ()
}

let updatePerson (connectionString: string) (msg: Message<ValidatedPerson>): Async<Result<unit, UpdatePersonError>> = async {
    do! Server.Persons.Storage.updatePerson connectionString msg.Payload
    return Ok ()
}