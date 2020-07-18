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
        FileName = reader.string "FileName"
        FileSize = reader.int "FileSize"
        MimeType = reader.string "MimeType"
        UploadedOn = new DateTimeOffset(DateTime.SpecifyKind(reader.dateTime "UploadedOn", DateTimeKind.Utc))
    }

    let selectQuery =
        """
            SELECT Partition, EntityId, FileId, FileName, FileSize, MimeType, UploadedOn
            FROM MediaFiles
        """


let getMediaFilesForEntities (connectionString: string) (partition: string) (entityIds: Guid list) =
    Sql.connect connectionString
    |> Sql.query (sprintf "%s WHERE Partition = @Partition AND EntityId in @EntityIds" selectQuery)
    |> Sql.parameters [ 
        "@EntityIds", Sql.uuidArray (entityIds |> List.toArray) 
        "@Partition", Sql.string partition
    ]
    |> Sql.read readMediaFile

let getMediaFileById (connectionString: string) (partition: string) (fileId: Guid) =
    Sql.connect connectionString
    |> Sql.query (sprintf "%s WHERE FileId = @FileId AND Partition = @Partition" selectQuery)
    |> Sql.parameters [ 
        "@FileId", Sql.uuid fileId 
        "@Partition", Sql.string partition
    ]
    |> Sql.readSingle readMediaFile
