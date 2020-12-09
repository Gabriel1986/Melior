module Server.StorageEngine.MediaStorage

open Npgsql.FSharp
open Server.Library
open Server.PostgreSQL
open Server.Blueprint.Data.Storage
open Shared.MediaLibrary

let private mediaFileToSqlProps (mediaFile: MediaFile) = [
    "@Partition", Sql.string mediaFile.Partition
    "@EntityId", Sql.uuid mediaFile.EntityId
    "@FileId", Sql.uuid mediaFile.FileId
    "@BuildingId", Sql.uuidOrNone mediaFile.BuildingId
    "@FileName", Sql.string mediaFile.FileName
    "@FileSize", Sql.int mediaFile.FileSize
    "@MimeType", Sql.string mediaFile.MimeType
    "@UploadedOn", Sql.timestamp mediaFile.UploadedOn.LocalDateTime
]

let transformEventToSql (msg: Message<MediaEvent>) =
    match msg.Payload with
    | MediaEvent.MediaFileWasCreated mediaFile ->
        [
            """
                INSERT INTO MediaFiles 
                    (Partition, EntityId, FileId, BuildingId, FileName, FileSize, MimeType, UploadedOn)
                VALUES 
                    (@Partition, @EntityId, @FileId, @BuildingId, @FileName, @FileSize, @MimeType, @UploadedOn)
            """, [ mediaFileToSqlProps mediaFile ]
        ]
    | MediaEvent.MediaFileWasPersisted mediaFileId ->
        [
            "UPDATE MediaFiles SET Status = 'Persisted' WHERE FileId = @MediaFileId"
            , [[ "@MediaFileId", Sql.uuid mediaFileId ]]
        ]
    | MediaEvent.MediaOfEntityWasDeleted entityId ->
        [
            "UPDATE MediaFiles SET Status = 'Deleted' WHERE EntityId = @EntityId"
            , [[ "@MediaFileId", Sql.uuid entityId ]]
        ]
    | MediaEvent.MediaFileWasDeleted mediaFileId ->
        [
            "UPDATE MediaFiles SET Status = 'Deleted' WHERE FileId = @MediaFileId"
            , [[ "@MediaFileId", Sql.uuid mediaFileId ]]
        ]
    | MediaEvent.TemporaryFileWasDeleted mediaFileId ->
        [
            "DELETE FROM MediaFiles WHERE FileId = @MediaFileId"
            , [[ "@MediaFileId", Sql.uuid mediaFileId ]]
        ]