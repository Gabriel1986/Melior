module Server.Media.Storage

open System
open Npgsql.FSharp

open Shared.MediaLibrary
open Server.PostgreSQL

[<NoEquality; NoComparison>]
type IMediaStorage =
    abstract CreateMediaFile: MediaFile -> Async<unit>
    abstract DeleteMediaFile: Guid -> Async<int>
    abstract DeleteMediaFilesForEntity: entityId: Guid -> Async<int>
    
let paramsFor (mediaFile: MediaFile) = [
    "@Partition", Sql.string mediaFile.Partition
    "@EntityId", Sql.uuid mediaFile.EntityId
    "@FileId", Sql.uuid mediaFile.FileId
    "@BuildingId", Sql.uuidOrNone mediaFile.BuildingId
    "@FileName", Sql.string mediaFile.FileName
    "@FileSize", Sql.int mediaFile.FileSize
    "@MimeType", Sql.string mediaFile.MimeType
    "@UploadedOn", Sql.timestamp mediaFile.UploadedOn.LocalDateTime
]

let createMediaFile (connectionString: string) (mediaFile: MediaFile) =
    Sql.connect connectionString
    |> Sql.query
        """
            INSERT INTO MediaFiles (
                Partition,
                EntityId,
                FileId,
                BuildingId,
                FileName,
                FileSize,
                MimeType,
                UploadedOn
            ) VALUES (
                @Partition,
                @EntityId,
                @FileId,
                @BuildingId,
                @FileName,
                @FileSize,
                @MimeType,
                @UploadedOn
            )
        """
    |> Sql.parameters (paramsFor mediaFile)
    |> Sql.writeAsync
    |> Async.Ignore

let deleteMediaFilesForEntity (connectionString: string) (entityId: Guid) =
    Sql.connect connectionString
    |> Sql.query "DELETE FROM MediaFiles WHERE EntityId = @EntityId"
    |> Sql.parameters [ "@MediaFileId", Sql.uuid entityId ]
    |> Sql.writeAsync

let deleteMediaFile (connectionString: string) (mediaFileId: Guid) =
    Sql.connect connectionString
    |> Sql.query "DELETE FROM MediaFiles WHERE FileId = @MediaFileId"
    |> Sql.parameters [ "@MediaFileId", Sql.uuid mediaFileId ]
    |> Sql.writeAsync

let makeStorage conn: IMediaStorage = {
    new IMediaStorage with
        member _.CreateMediaFile mediaFile = createMediaFile conn mediaFile
        member _.DeleteMediaFile mediaFileId = deleteMediaFile conn mediaFileId
        member _.DeleteMediaFilesForEntity entityId = deleteMediaFilesForEntity conn entityId
}