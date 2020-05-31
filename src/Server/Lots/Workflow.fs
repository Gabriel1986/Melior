module Server.Lots.Workflow

open Shared.Remoting

let createLot connectionString message = async {
    return Ok ()
}

let updateLot connectionString message = async {
    return Ok ()
}

let deleteLot connectionString message: Async<Result<unit, AuthorizationError>> = async {
    return Ok ()
}
