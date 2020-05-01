namespace Server.Buildings

open Shared.Domain

module Workflow =
    let createBuilding storage msg: Async<Result<unit, InvariantError>> =
        async { return Ok () }

    let updateBuilding storage msg: Async<Result<unit, InvariantError>> =
        async { return Ok () }

    let deleteBuilding storage msg: Async<Result<unit, InvariantError>> =
        async { return Ok () }
