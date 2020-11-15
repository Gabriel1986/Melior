module Server.Media.Workflow

open System
open Shared.MediaLibrary
open Server.Library
open Storage

let createMediaFile (storage: IMediaStorage) (msg: Message<MediaFile>): Async<unit> =
    storage.CreateMediaFile msg.Payload

let deleteMediaFilesForEntity (storage: IMediaStorage) (msg: Message<Guid>) =
    storage.DeleteMediaFilesForEntity msg.Payload
    |> Async.Ignore

let deleteMediaFile (storage: IMediaStorage) (msg: Message<Guid>) =
    storage.DeleteMediaFile msg.Payload
    |> Async.Ignore