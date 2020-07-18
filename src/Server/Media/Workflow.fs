module Server.Media.Workflow

open System
open Shared.MediaLibrary
open Server.Library

let createMediaFile (connectionString: string) (msg: Message<MediaFile>): Async<unit> =
    Server.Media.Storage.createMediaFile connectionString msg.Payload

let deleteMediaFilesForEntity (connectionString: string) (msg: Message<Guid>) =
    Server.Media.Storage.deleteMediaFilesForEntity connectionString msg.Payload

let deleteMediaFile (connectionString: string) (msg: Message<Guid>): Async<unit> =
    Server.Media.Storage.deleteMediaFile connectionString msg.Payload