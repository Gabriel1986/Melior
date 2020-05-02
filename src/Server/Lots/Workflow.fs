namespace Server.Lots

open Shared.Domain

module Workflow =
    let deleteLot connectionString message: Async<Result<unit, InvariantError>> = async {
        return Ok ()
    }
