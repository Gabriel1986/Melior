module Server.Contracts.Workflow

open System
open Shared.Read
open Shared.Write
open Shared.Remoting
open Server.Library
open Server.LibraryExtensions
open Server.Blueprint.Behavior.Storage
open Server.Blueprint.Data.Storage

let createContract (store: IStorageEngine) (msg: Message<Contract>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding msg.Payload.BuildingId then
        let validated = ValidatedContract.Validate msg.Payload
        match validated with
        | Ok validated ->
            let! _ = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Created
                |> ContractEvent.ContractEvent
                |> StorageEvent.ContractEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveContractError.Validation validationErrors)
    else
        return Error SaveContractError.AuthorizationError
}

let updateContract (store: IStorageEngine) (msg: Message<Contract>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding msg.Payload.BuildingId then
        let validated = ValidatedContract.Validate msg.Payload
        match validated with
        | Ok validated ->
            let! nbRows = 
                store.PersistTransactional [
                    validated
                    |> BuildingSpecificCUDEvent.Updated
                    |> ContractEvent.ContractEvent
                    |> StorageEvent.ContractEvent
                    |> inMsg msg
                ]
            if nbRows = 0 then
                return Error SaveContractError.NotFound
            else
                return Ok ()
        | Error validationErrors ->
            return Error (SaveContractError.Validation validationErrors)
    else
        return Error SaveContractError.AuthorizationError
}

let deleteContract (store: IStorageEngine) (msg: Message<BuildingId * Guid>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload) then
        let! nbRows = 
            store.PersistTransactional [
                (msg.Payload)
                |> BuildingSpecificCUDEvent.Deleted
                |> ContractEvent.ContractEvent
                |> StorageEvent.ContractEvent
                |> inMsg msg
            ]
        if nbRows = 0 then
            return Error DeleteContractError.NotFound
        else
            return Ok ()
    else
        return Error DeleteContractError.AuthorizationError
}

let saveContractTypeAnswer (store: IStorageEngine) (msg: Message<ContractTypeAnswer list>) = async {
    if msg.Payload |> List.forall (fun answer -> msg.CurrentUser.HasAdminAccessToBuilding answer.BuildingId) then
        let! _ = 
            store.PersistTransactional [
                msg.Payload
                |> ContractEvent.ContractTypeAnswersWereUpdated
                |> StorageEvent.ContractEvent
                |> inMsg msg
            ]
        return Ok ()
    else
        return Error SaveAnswerError.AuthorizationError
}