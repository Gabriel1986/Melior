module Server.Buildings.Workflow

open System
open Shared.Remoting
open Shared.Write
open Server.Library

type SharedConciergeId = Shared.Write.ConciergeId
type SharedSyndicId = Shared.Write.SyndicId

let createBuilding (connectionString: string) (msg: Message<ValidatedBuilding>): Async<Result<unit, CreateBuildingError>> = async {
    do! Server.Buildings.Storage.createBuilding connectionString msg.Payload
    return Ok ()
}

let updateBuilding (connectionString) (msg: Message<ValidatedBuilding>): Async<Result<unit, UpdateBuildingError>> = async {
    do! Server.Buildings.Storage.updateBuilding connectionString msg.Payload
    return Ok ()
}

let deleteBuilding (connectionString: string) (msg: Message<Guid>): Async<Result<unit, AuthorizationError>> = async { 
    do! Server.Buildings.Storage.deleteBuilding connectionString msg.Payload
    return Ok () 
}

let updateBuildingSyndic (connectionString: string) (msg: Message<Guid * SyndicId option>) = async {
    do! Server.Buildings.Storage.updateBuildingSyndic connectionString msg.Payload
    return ()
}

let updateBuildingConcierge (connectionString: string) (msg: Message<Guid * ConciergeId option>) = async {
    do! Server.Buildings.Storage.updateBuildingConcierge connectionString msg.Payload
    return ()
}