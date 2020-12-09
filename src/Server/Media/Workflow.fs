module Server.Media.Workflow

open System
open Shared.Read
open Shared.MediaLibrary
open Server.Library
open Server.Blueprint.Behavior.Storage
open Server.Blueprint.Data.Storage

let createMediaFile (storage: IStorageEngine) (msg: Message<MediaFile>): Async<unit> =
    storage.PersistTransactional [
        msg.Payload
        |> MediaEvent.MediaFileWasCreated
        |> StorageEvent.MediaEvent
        |> inMsg msg
    ]
    |> Async.Ignore

let deleteMediaFilesForEntity (storage: IStorageEngine) (msg: Message<Guid>) =
    storage.PersistTransactional [
        msg.Payload
        |> MediaEvent.MediaOfEntityWasDeleted
        |> StorageEvent.MediaEvent
        |> inMsg msg
    ]
    |> Async.Ignore

let deleteMediaFile (storage: IStorageEngine) (msg: Message<Guid>) =
    storage.PersistTransactional [
        msg.Payload
        |> MediaEvent.MediaFileWasDeleted
        |> StorageEvent.MediaEvent
        |> inMsg msg
    ]
    |> Async.Ignore

let removeTemporaryMediaFile (storage: IStorageEngine) (msg: Message<Guid>) =
    storage.PersistTransactional [
        msg.Payload
        |> MediaEvent.TemporaryFileWasDeleted
        |> StorageEvent.MediaEvent
        |> inMsg msg
    ]
    |> Async.Ignore