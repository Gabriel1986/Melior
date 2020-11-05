module Server.Contracts.Workflow

open System
open Shared.Read
open Shared.Write
open Shared.Remoting
open Server.Library
open Server.LibraryExtensions
open Storage

let createContract (store: IContractStorage) (msg: Message<Contract>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding msg.Payload.BuildingId then
        let validated = ValidatedContract.Validate msg.Payload
        match validated with
        | Ok validated -> 
            let! ok = store.CreateContract (msg |> Message.map validated)
            return Ok ok
        | Error validationErrors ->
            return Error (SaveContractError.Validation validationErrors)
    else
        return Error SaveContractError.AuthorizationError
}

let updateContract (store: IContractStorage) (msg: Message<Contract>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding msg.Payload.BuildingId then
        let validated = ValidatedContract.Validate msg.Payload
        match validated with
        | Ok validated ->
            let! nbRows = store.UpdateContract (msg |> Message.map validated)
            if nbRows = 0 then
                return Error SaveContractError.NotFound
            else
                return Ok ()
        | Error validationErrors ->
            return Error (SaveContractError.Validation validationErrors)
    else
        return Error SaveContractError.AuthorizationError
}

let deleteContract (store: IContractStorage) (msg: Message<BuildingId * Guid>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding (fst msg.Payload) then
        let! nbRows = store.DeleteContract msg
        if nbRows = 0 then
            return Error DeleteContractError.NotFound
        else
            return Ok ()
    else
        return Error DeleteContractError.AuthorizationError
}

let saveContractTypeAnswer (store: IContractStorage) (msg: Message<ContractTypeAnswer>) = async {
    if msg.CurrentUser.HasAdminAccessToBuilding msg.Payload.BuildingId then
        let! nbRows = store.SaveContractTypeAnswer msg.Payload
        if nbRows = 0 then
            return Error SaveAnswerError.NotFound
        else
            return Ok ()
    else
        return Error SaveAnswerError.AuthorizationError
}