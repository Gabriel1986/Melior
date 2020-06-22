module Server.ProfessionalSyndics.Workflow

open System
open Shared.Remoting
open Shared.Write
open Server.Library

let createProfessionalSyndic connectionString (message: Message<ValidatedProfessionalSyndic>) = async {
    do! Server.ProfessionalSyndics.Storage.createProfessionalSyndic connectionString message.Payload
    return Ok()
}

let updateProfessionalSyndic connectionString (message: Message<ValidatedProfessionalSyndic>) = async {
    do! Server.ProfessionalSyndics.Storage.updateProfessionalSyndic connectionString message.Payload
    return Ok()
}

let deleteProfessionalSyndic connectionString (message: Message<Guid>): Async<Result<unit, AuthorizationError>> = async {
    do! Server.ProfessionalSyndics.Storage.deleteProfessionalSyndic connectionString message.Payload
    return Ok ()
}