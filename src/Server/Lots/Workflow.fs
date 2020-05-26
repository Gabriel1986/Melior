module Server.Lots.Workflow

open Shared.Remoting

let deleteLot connectionString message: Async<Result<unit, AuthorizationError>> = async {
    return Ok ()
}
