module Client.Upload

open System
open Fable.React
open Fable.Core.JsInterop
open Fable.Core
open Browser
open Shared.MediaLibrary

importAll "./public/FilePond/filepond.min.css"
importAll "./public/FilePond/filepond-plugin-image-preview.min.css"

type Status =
    | Empty = 0
    | Idle = 1
    | Error = 2
    | Busy = 3
    | Ready = 4

type FileStatus =
    | Init = 1
    | Idle = 2
    | ProcessingQueued = 9
    | Processing = 3
    | ProcessingComplete = 5
    | ProcessingError = 6
    | ProcessingRevertError = 10
    | Loading = 7
    | LoadError = 8

type FilePondFile = {
      id: string
      serverId: string
      //input, limbo or origin
      origin: string
      status: FileStatus
      file: FilePondFile
      fileExtension: string
      fileSize: int
      filename: string
      filenameWithoutExtension: string
      fileType: string
}

module FilePondFile =
    let toMediaFile (partition: string) (buildingId: Guid option) (entityId: Guid) (filePondFile: FilePondFile): MediaFile = {
        Partition = partition
        EntityId = entityId
        BuildingId = buildingId
        FileId = Guid.Parse (filePondFile.serverId)
        FileName = filePondFile.filename
        FileSize = filePondFile.fileSize
        MimeType = MimeTypes.getMimeType (filePondFile.fileExtension)
        UploadedOn = DateTimeOffset.Now
    }

type InitialFile = {| source: Guid; options: {| ``type``: string |} |}

type ServerOptions =
    {|
        url: obj
        ``process``: obj
        revert: obj
        patch: obj
        fetch: obj
        load: obj
        headers: obj
    |}

type FilePondOptions =
    | MaxFiles of int
    | AllowMultiple of bool
    | Disabled of bool
    | Server of obj
    | ChunkSize of double
    | ServerOptions of ServerOptions
    | OnProcessFile of (string -> FilePondFile -> unit) //Finished uploading a file callback
    | OnRemoveFile of (string -> FilePondFile -> unit) //Removed an uploaded file callback
    | AcceptedFileTypes of string array
    | LabelIdle of string
    | InitialFiles of Guid list
    | Files of InitialFile array

let private htmlBasePath = document.baseURI.TrimEnd [| '/' |]

let downloadUri (partition: string) (fileId: System.Guid): string =
    sprintf "%s/media/download/%s/%O" htmlBasePath partition fileId

[<Emit("window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth")>]
let width: float = jsNative

let thumbnailUri (partition: string) (fileId: System.Guid): string =
    if width < 800.0
    then sprintf "%s/media/thumbnail/small/%s/%O" htmlBasePath partition fileId
    else sprintf "%s/media/thumbnail/large/%s/%O" htmlBasePath partition fileId

let inline filePondComponent (props : obj): ReactElement =
    ofImport "FilePondComponent" "./public/FilePond/FilePond.js" props []

let private defaultServerOptions (partition: string) (buildingId: System.Guid option) (entityId: Guid): ServerOptions = 
    let uploadPath = sprintf "/upload/%s/%A" partition entityId
    let downloadPath = sprintf "/download/%s" partition
    {|
        url = sprintf "%s/media" htmlBasePath
        ``process`` = uploadPath
        revert = uploadPath
        patch = uploadPath + "/"
        fetch = uploadPath + "/"
        load = downloadPath + "/"
        headers = {| BuildingId = buildingId |}
    |}

//I would have done it a smarter way in English, but in other languages it might be more of a difference than just 'file' vs 'files'
let defaultIdleLabelForMultiple = "Versleep je bestanden of <span class='filepond--label-action'> Browse </span>"
let defaultIdleLabelForSingular = "Versleep je bestand of <span class='filepond--label-action'> Browse </span>"

let determineIdleLabel (options: FilePondOptions list): string =
    options
    |> List.tryPick (fun opt -> match opt with | AllowMultiple allow -> (if allow then Some defaultIdleLabelForMultiple else Some defaultIdleLabelForSingular) | _ -> None)
    |> Option.defaultValue defaultIdleLabelForMultiple


let renderFilePond (props: {| Partition: string; BuildingId: System.Guid option; EntityId: System.Guid; Options: FilePondOptions list |}): ReactElement =
    let partition = props.Partition
    let entityId = props.EntityId
    let buildingId = props.BuildingId
    let options = props.Options

    let idleLabel =
        options
        |> List.tryPick (fun opt -> match opt with | LabelIdle idleLbl -> Some idleLbl | _ -> None)
        |> Option.defaultWith (fun _ -> determineIdleLabel options)

    let chunkSize =
        options
        |> List.tryPick (fun opt -> match opt with | ChunkSize size -> Some size | _ -> None)
        |> Option.defaultValue DefaultChunkSizeInBytes

    let initialFiles =
        options
        |> List.collect (fun opt -> match opt with | InitialFiles fileIds -> fileIds | _ -> [])
        |> List.map (fun fileId -> {| source = fileId; options = {| ``type`` = "local" |} |})
        |> List.toArray

    let serverOptions =
        match options |> List.tryPick (function | ServerOptions opt -> Some opt | _ -> None) with
        | None -> defaultServerOptions partition buildingId entityId
        | Some options -> options

    let otherFilePondOptions =
        options
        |> List.filter (
            function 
            | LabelIdle _ -> false 
            | ServerOptions _ -> false 
            | ChunkSize _ -> false 
            | InitialFiles _ -> false
            | _ -> true)

    [
        yield Server serverOptions
        yield LabelIdle idleLabel
        yield ChunkSize chunkSize
        yield Files initialFiles
        yield! otherFilePondOptions
    ]
    |> keyValueList CaseRules.LowerFirst
    |> filePondComponent

let filePond =
    FunctionComponent.Of (renderFilePond, "FilePondUpload", memoEqualsButFunctions)