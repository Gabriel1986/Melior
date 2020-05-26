module Server.Owners.Workflow

open Shared.Remoting

let deleteOwner connectionString message: Async<Result<unit, AuthorizationError>> = async {
    return Ok ()
}