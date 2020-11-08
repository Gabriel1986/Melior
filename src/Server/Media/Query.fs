module Server.Media.Query

open System
open Npgsql.FSharp

open Shared.MediaLibrary
open Server.PostgreSQL
open Server.PostgreSQL.Sql

[<AutoOpen>]
module private Readers =
    let readMediaFile (reader: CaseInsensitiveRowReader): MediaFile = {
        Partition = reader.string "Partition"
        EntityId = reader.uuid "EntityId"
        FileId = reader.uuid "FileId"
        BuildingId = reader.uuidOrNone "BuildingId"
        FileName = reader.string "FileName"
        FileSize = reader.int "FileSize"
        MimeType = reader.string "MimeType"
        UploadedOn =
            let uploadedOn = reader.dateTime "UploadedOn"
            new DateTimeOffset(uploadedOn.Year, uploadedOn.Month, uploadedOn.Day, uploadedOn.Hour, uploadedOn.Minute, uploadedOn.Second, TimeSpan.FromHours(2.0))
    }

    let selectQuery =
        """
            SELECT Partition, EntityId, FileId, BuildingId, FileName, FileSize, MimeType, UploadedOn
            FROM MediaFiles
        """

let getMediaFilesForEntities (connectionString: string) (partition: string) (entityIds: Guid list) =
    Sql.connect connectionString
    |> Sql.query (sprintf "%s WHERE Partition = @Partition AND EntityId = ANY (@EntityIds)" selectQuery)
    |> Sql.parameters [ 
        "@EntityIds", Sql.uuidArray (entityIds |> List.toArray) 
        "@Partition", Sql.string partition
    ]
    |> Sql.read readMediaFile

let getMediaFilesByIds (conn: string) (partition: string) (fileIds: Guid list) =
    Sql.connect conn
    |> Sql.query (sprintf "%s WHERE FileId = ANY (@FileIds) AND Partition = @Partition" selectQuery)
    |> Sql.parameters [ 
        "@FileIds", Sql.uuidArray (fileIds |> Array.ofList)
        "@Partition", Sql.string partition
    ]
    |> Sql.read readMediaFile

let getMediaFileById (conn: string) (partition: string) (fileId: Guid) =
    getMediaFilesByIds conn partition [ fileId ]
    |> Async.map List.tryHead