module Server.Lots.Workflow

open System
open Shared.Remoting
open Shared.Read
open Shared.Write
open Shared.ConstrainedTypes
open Server.Library
open Server.LibraryExtensions
open Server.Blueprint.Behavior.Storage
open Server.Blueprint.Data.Storage

let createLot (storage: IStorageEngine) (msg: Message<Lot>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId)
    then
        let validated = ValidatedLot.Validate msg.Payload
        match validated with
        | Ok validated ->
            let! _ = storage.PersistTransactional [
                yield
                    validated
                    |> BuildingSpecificCUDEvent.Created
                    |> LotEvent.LotEvent
                    |> StorageEvent.LotEvent
                    |> inMsg msg

                yield!
                    validated.Owners
                    |> List.collect (fun owner -> [
                        (validated, owner)
                        |> BuildingSpecificCUDEvent.Created
                        |> LotEvent.LotOwnerEvent
                        |> StorageEvent.LotEvent
                        |> inMsg msg
                    ])
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveLotError.Validation validationErrors)
    else
        return Error SaveLotError.AuthorizationError
}

let updateLot (storage: IStorageEngine) (getLot: Guid -> Async<Lot option>) (msg: Message<Lot>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (msg.Payload.BuildingId)
    then
        let validated = ValidatedLot.Validate msg.Payload
        match validated with
        | Ok validated ->
            let! currentLotOpt = getLot validated.LotId
            match currentLotOpt |> Option.map ValidatedLot.Validate with
            | Some (Ok currentLot) ->
                let createdLotOwners, updatedLotOwners =
                    validated.Owners
                    |> List.fold (fun (created, updated) next ->
                        match currentLot.Owners |> List.tryFind (fun o -> o.LotOwnerId = next.LotOwnerId) with
                        | Some matchingLotOwner ->
                            if next <> matchingLotOwner 
                            then (created, next::updated)
                            else (created, updated) 
                        | None ->
                            (next::created, updated)
                    ) ([], [])

                let deletedLotOwners =
                    let currentSet = currentLot.Owners |> List.map (fun o -> o.LotOwnerId) |> Set.ofList
                    let updatedSet = validated.Owners  |> List.map (fun o -> o.LotOwnerId) |> Set.ofList
                    (currentSet - updatedSet)
                    |> Set.toList

                let! nbRowsAffected = storage.PersistTransactional [
                    yield
                        validated
                        |> BuildingSpecificCUDEvent.Updated
                        |> LotEvent.LotEvent
                        |> StorageEvent.LotEvent
                        |> inMsg msg

                    yield!
                        createdLotOwners
                        |> List.collect (fun owner -> [
                            (validated, owner)
                            |> BuildingSpecificCUDEvent.Created
                            |> LotEvent.LotOwnerEvent
                            |> StorageEvent.LotEvent
                            |> inMsg msg
                        ])

                    yield!
                        updatedLotOwners
                        |> List.collect (fun owner -> [
                            (validated, owner)
                            |> BuildingSpecificCUDEvent.Updated
                            |> LotEvent.LotOwnerEvent
                            |> StorageEvent.LotEvent
                            |> inMsg msg
                        ])

                    yield!
                        deletedLotOwners
                        |> List.map (fun lotOwnerId ->
                            (validated.BuildingId, lotOwnerId)
                            |> BuildingSpecificCUDEvent.Deleted
                            |> LotEvent.LotOwnerEvent
                            |> StorageEvent.LotEvent
                            |> inMsg msg
                        )
                ]
                if nbRowsAffected = 0
                then return Error (SaveLotError.NotFound)
                else return Ok ()
            | Some (Error e) ->
                return failwithf "Precondition failed, could not validate lot that was already stored: %A" e
            | None ->
                return Error (SaveLotError.NotFound)
        | Error validationErrors ->
            return Error (SaveLotError.Validation validationErrors)
    else
        return Error SaveLotError.AuthorizationError
}

let deleteLot (storage: IStorageEngine) (msg: Message<BuildingId * Guid>): Async<Result<unit, DeleteLotError>> = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload)
    then
        let! nbRowsAffected = storage.PersistTransactional [
            msg.Payload
            |> BuildingSpecificCUDEvent.Deleted
            |> LotEvent.LotEvent
            |> StorageEvent.LotEvent
            |> inMsg msg
        ]
        if nbRowsAffected = 0
        then return Error DeleteLotError.NotFound
        else return Ok ()
    else
        return Error DeleteLotError.AuthorizationError
}

let generateOGMReferences (storage: IStorageEngine) (getLotOwners: unit -> Async<FinancialLotOwner list>): Async<unit> = async {
    let! lotOwners = getLotOwners ()
    let inMsg (payload: 'a): Message<'a> = {
        CreatedAt = DateTimeOffset.Now
        Context = None
        Payload = payload
    }
    let events =
        lotOwners |> List.map (fun lotOwner ->
            let newOGM = BelgianOGM.Generate ()
            (lotOwner.LotOwnerId, newOGM)
            |> LotEvent.LotOwnerOGMReferenceWasUpdated
            |> StorageEvent.LotEvent
            |> inMsg
        )
    return! storage.PersistTransactional events |> Async.Ignore
}