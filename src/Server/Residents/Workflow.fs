namespace Server.Residents

open Shared.Domain

module Workflow =
    let deleteResident connectionString message: Async<Result<unit, InvariantError>> = async {
        return Ok ()
    }