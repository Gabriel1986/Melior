module Server.Media.MediaSystem

open Microsoft.Extensions.Configuration
open Server.Blueprint.Behavior.Media
open Server.Blueprint.Behavior.Storage
open Server.Blueprint.Data.Storage
open Server.AppSettings
open Shared.Library
open Shared.MediaLibrary

let build (config: IConfiguration) (store: IStorageEngine): IMediaSystem = 
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection
    {
        new IMediaSystem with
            member _.CreateMediaFile msg = Workflow.createMediaFile store msg
            member _.DeleteMediaFilesForEntity msg = Workflow.deleteMediaFilesForEntity store msg
            member _.DeleteMediaFile msg = Workflow.deleteMediaFile store msg
            member _.RemoveTemporaryMediaFile msg = Workflow.removeTemporaryMediaFile store msg
            member _.GetMediaFile partition fileId = Query.getMediaFileById conn partition fileId
            member _.GetMediaFilesForEntities partition entityIds = Query.getMediaFilesForEntities conn partition entityIds
            member _.GetAllMediaFiles () = Query.getAllMediaFiles conn
            member me.HttpHandler = HttpHandler.mediaHandler config me
    }

type ReactiveBehavior (config: IConfiguration) =
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection

    interface IReactiveBehavior with
        override _.ReactTo message =
            match message.Payload with
            | StorageEvent.BuildingEvent (BuildingEvent.BuildingEvent (CUDEvent.Created validated))
            | StorageEvent.BuildingEvent (BuildingEvent.BuildingEvent (CUDEvent.Updated validated)) ->
                async {
                    let! currentMediaFiles = Query.getMediaFilesForEntities conn Partitions.BuildingImages [ validated.BuildingId ]
                    let mediaFilesToDelete =
                        match validated.PictureId with
                        | Some fileId -> currentMediaFiles |> List.map (fun file -> file.FileId) |> List.filter ((<>) fileId)
                        | None -> currentMediaFiles |> List.map (fun file -> file.FileId)
                    let deleteEvents = mediaFilesToDelete |> List.map (fun fileId -> StorageEvent.MediaEvent (MediaEvent.MediaFileWasDeleted fileId))
                    let persistEvents =
                        match validated.PictureId with
                        | Some fileId -> [ StorageEvent.MediaEvent (MediaEvent.MediaFileWasPersisted fileId) ]
                        | None -> []
                    return deleteEvents @ persistEvents
                }
            | StorageEvent.ContractEvent (ContractEvent.ContractEvent (Created validated))
            | StorageEvent.ContractEvent (ContractEvent.ContractEvent (Updated validated)) ->
                async {
                    let! currentMediaFiles = Query.getMediaFilesForEntities conn Partitions.Contracts [ validated.ContractId ]
                    let currentMediaFileSet = currentMediaFiles |> List.map (fun file -> file.FileId) |> Set.ofList
                    let updatedMediaFileSet = validated.ContractFileIds |> Set.ofList

                    let deletedFileIds = currentMediaFileSet - updatedMediaFileSet |> Set.toList
                    let newFileIds = updatedMediaFileSet - currentMediaFileSet |> Set.toList

                    let deleteEvents =
                        deletedFileIds
                        |> List.map (fun fileId -> StorageEvent.MediaEvent (MediaEvent.MediaFileWasDeleted fileId))
                    let persistEvents = 
                        newFileIds
                        |> List.map (fun fileId -> StorageEvent.MediaEvent (MediaEvent.MediaFileWasPersisted fileId))
                    return deleteEvents @ persistEvents
                }
            | StorageEvent.FinancialEvent (FinancialEvent.InvoiceEvent (Created validated))
            | StorageEvent.FinancialEvent (FinancialEvent.InvoiceEvent (Updated validated)) ->
                async {
                    let! currentMediaFiles = Query.getMediaFilesForEntities conn Partitions.Invoices [ validated.InvoiceId ]
                    let currentMediaFileSet = currentMediaFiles |> List.map (fun file -> file.FileId) |> Set.ofList
                    let updatedMediaFileSet = validated.MediaFileIds |> Set.ofList

                    let deletedFileIds = currentMediaFileSet - updatedMediaFileSet |> Set.toList
                    let newFileIds = updatedMediaFileSet - currentMediaFileSet |> Set.toList

                    let deleteEvents =
                        deletedFileIds
                        |> List.map (fun fileId -> StorageEvent.MediaEvent (MediaEvent.MediaFileWasDeleted fileId))
                    let persistEvents = 
                        newFileIds
                        |> List.map (fun fileId -> StorageEvent.MediaEvent (MediaEvent.MediaFileWasPersisted fileId))
                    return deleteEvents @ persistEvents
                }
            | _ ->
                Async.lift []