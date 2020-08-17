module Server.Media.HttpHandler
open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.Extensions.Configuration
open System
open System.IO
open Microsoft.AspNetCore.Http
open Giraffe
open SixLabors.ImageSharp;
open SixLabors.ImageSharp.Processing;
open SixLabors.ImageSharp.Formats.Jpeg
open Amazon.S3
open Amazon.S3.Transfer
open Amazon.S3.Model

open Shared.MediaLibrary

open Server.Library
open Server.Blueprint.Behavior.Media

[<AutoOpen>]
module private Internals =
    let createAmazonS3ServiceClient (config: IConfiguration) =
#if DEBUG
        let awsConfig = new Amazon.S3.AmazonS3Config()
        awsConfig.ServiceURL <- "http://localhost:4572"
        awsConfig.UseHttp <- true
        awsConfig.ForcePathStyle <- true //Can't set this using the GetAWSOptions... Needed to do this manually for development....
        new AmazonS3Client (awsConfig) :> IAmazonS3
#else
        let awsConfig = config.GetAWSOptions()
        let serviceClient = awsConfig.CreateServiceClient<IAmazonS3>()
        serviceClient
#endif

    let createMessage (ctx: HttpContext) (data: 'T) : Message<'T> = { 
        Context = ctx
        Payload = data
        CreatedAt = DateTimeOffset.Now
    }

    let temporaryPath = Path.GetTempPath()

    let getChunkDirectory partition fileId =
        Path.Combine(temporaryPath, partition, string fileId)

    let post (partition: string, _entityId: Guid) =
        fun nxt (ctx: HttpContext) ->
            task {
                let fileId = Guid.NewGuid()
                printfn "Generating a new file id: %O" fileId
                let chunkDirectory = getChunkDirectory partition fileId

                printfn "Generating chunk directory on path: %s" chunkDirectory
                if (not (Directory.Exists(chunkDirectory))) then Directory.CreateDirectory(chunkDirectory) |> ignore
                return! text (string fileId) nxt ctx
            }

    let upload (config: IConfiguration) (system: IMediaSystem)  (partition: string, entityId: Guid, fileId: Guid) =
        fun nxt (ctx : HttpContext) ->
            task {
                match ctx.Request.ContentType with
                | "application/offset+octet-stream" ->
                    printfn "Uploading started."
                    let getHeaderValue headerType = ctx.Request.Headers.[headerType].[0]
                    let uploadLength = getHeaderValue "Upload-Length" |> Double.Parse
                    let uploadName = getHeaderValue "Upload-Name"
                    let uploadOffset = getHeaderValue "Upload-Offset" |> Double.Parse

                    //Generate file path
                    let temporaryFileNumber = uploadOffset / DefaultChunkSizeInBytes |> int
                    let chunkDirectoryPath = getChunkDirectory partition fileId
                    let chunkFilePath = Path.Combine(chunkDirectoryPath, sprintf "%s.%i" uploadName temporaryFileNumber)
                    printfn "Downloading chunk to: %s" chunkFilePath

                    //Stream to temporary file
                    use stream = new FileStream(chunkFilePath, FileMode.OpenOrCreate)
                    do! ctx.Request.Body.CopyToAsync stream
                    stream.Close()

                    if uploadLength - uploadOffset < DefaultChunkSizeInBytes
                    then
                        printfn "Finished uploading. Combining file into a single file."
                        let chunkDirectory = DirectoryInfo (chunkDirectoryPath)
                        let combinedPath = Path.Combine(chunkDirectoryPath, sprintf "%s" uploadName)
                        let temporaryFiles = chunkDirectory.GetFiles ()

                        let openReadStreams =
                            temporaryFiles
                            |> Array.sortBy
                                (fun fileInfo ->
                                    fileInfo.Extension.Split(".")
                                    |> Array.last
                                    |> Int32.Parse)
                            |> List.ofArray
                            |> List.map (fun each -> fun () -> each.OpenRead() :> Stream)

                        //Write the file to the final destination on the filesystem
                        do!
                            task {
                                use writeStream = new FileStream(combinedPath, FileMode.OpenOrCreate)
                                for openReadStream in openReadStreams do
                                    use readStream = openReadStream()
                                    do! readStream.CopyToAsync(writeStream)
                            }
                        printfn "Files combined on path: %s" combinedPath

                        printfn "Creating meta data file"
                        //Create a meta data file for the DB
                        let file: MediaFile =
                            let fileExtension = uploadName.Split('.') |> Array.last
                            {
                                Partition = partition
                                EntityId = entityId
                                FileId = fileId
                                FileName = uploadName
                                FileSize = uploadLength |> int
                                MimeType = MimeTypes.getMimeType(fileExtension)
                                UploadedOn = DateTimeOffset.Now
                            }


                        printfn "Uploading file to S3"
                        //Upload the file to S3
                        //TODO: downsize PDF files
                        if file.IsImage 
                        then
                            //Generate small and large thumbnails
                            use inputStream = FileInfo(combinedPath).OpenRead()
                            let! originalImage = Image.LoadAsync(inputStream)
                            let largeThumbResizeOptions = 
                                new ResizeOptions(Mode = ResizeMode.Pad, Size = new Size(Height = 400, Width = 400))
                            let largeThumbnail = originalImage.Clone(fun x -> x.Resize(largeThumbResizeOptions) |> ignore)
                            let smallThumbResizeOptions =
                                new ResizeOptions(Mode = ResizeMode.Pad, Size = new Size(Height = 200, Width = 200))
                            let smallThumbnail = originalImage.Clone(fun x -> x.Resize(smallThumbResizeOptions) |> ignore)

                            let jpegEncoder = new JpegEncoder(Quality = Nullable(80));
                            use largeThumbnailStream = new MemoryStream()
                            largeThumbnail.Save(largeThumbnailStream, jpegEncoder)

                            use smallThumbnailStream = new MemoryStream()
                            smallThumbnail.Save(smallThumbnailStream, jpegEncoder)

                            use client = createAmazonS3ServiceClient config
                            do! client.UploadObjectFromFilePathAsync(partition, string fileId, combinedPath, [] |> dict)
                            do! client.UploadObjectFromStreamAsync(partition, sprintf "%O_%s" fileId "large", largeThumbnailStream, [] |> dict)
                            do! client.UploadObjectFromStreamAsync(partition, sprintf "%O_%s" fileId "small", smallThumbnailStream, [] |> dict)
                        else
                            printfn "Uploading file to S3 -> creating client"
                            use client = createAmazonS3ServiceClient config
                            printfn "Uploading object from file path async."
                            printfn "Service url: %s" client.Config.ServiceURL
                            printfn "Using HTTP: %O" client.Config.UseHttp
                            do! client.UploadObjectFromFilePathAsync(partition, string fileId, combinedPath, [] |> dict)

                        //Store the file metadata
                        printfn "Storing file metadata in the database"
                        do!
                            file
                            |> createMessage ctx
                            |> system.CreateMediaFile

                        //Temporary files are no longer necessary -> They're in S3 now
                        printfn "Deleting temporary directory for chunks and combined file"
                        (new DirectoryInfo(chunkDirectoryPath)).Delete(true)
                        return! setStatusCode (Net.HttpStatusCode.OK |> int) nxt ctx
                    else
                        return! setStatusCode (Net.HttpStatusCode.OK |> int) nxt ctx
                | _other ->
                    return! setStatusCode (Net.HttpStatusCode.Forbidden |> int) nxt ctx
            }

    let delete (config: IConfiguration) (system: IMediaSystem)  (partition: string, _entityId: Guid) =
        fun nxt (ctx: HttpContext) ->
            task {
                let! body = ctx.ReadBodyFromRequestAsync()
                let fileId = Guid body
                let chunkDirectoryInfo = DirectoryInfo (getChunkDirectory partition fileId)

                use client = createAmazonS3ServiceClient config
                let deleteObjectsRequest = new DeleteObjectsRequest(BucketName = partition)
                deleteObjectsRequest.AddKey(sprintf "%O"    fileId)
                deleteObjectsRequest.AddKey(sprintf "%O_%s" fileId "large")
                deleteObjectsRequest.AddKey(sprintf "%O_%s" fileId "small")

                //TODO: do something with the response? do we care about the response?
                let! response = client.DeleteObjectsAsync(deleteObjectsRequest)
                    
                do!
                    fileId
                    |> createMessage ctx
                    |> system.DeleteMediaFile  
                if (chunkDirectoryInfo.Exists) then chunkDirectoryInfo.Delete(true)
                return! setStatusCode (Net.HttpStatusCode.OK |> int) nxt ctx
            }

    //See FilePond docs -> server responds with Upload-Offset set to the next expected chunk offset in bytes.
    let determineUploadStatus (partition: string, _entityId: Guid, fileId: Guid) =
        let chunkDirectory = DirectoryInfo (getChunkDirectory partition fileId)
        if (not chunkDirectory.Exists)
        then
            setHttpHeader "Upload-Offset" 0.0f
        else
            let latestChunkFileIndex =
                chunkDirectory.GetFiles ()
                |> Array.map (fun fileInfo -> fileInfo.Extension.Split('.') |> Array.last |> int)
                |> Array.max
                |> float
            setHttpHeader "Upload-Offset" ((latestChunkFileIndex + 1.0) * DefaultChunkSizeInBytes)
        >=> setStatusCode (Net.HttpStatusCode.OK |> int)

    let download (config: IConfiguration) (system: IMediaSystem)  (partition: string, fileId: Guid) =
        fun nxt (ctx: HttpContext) ->
            task {
                match! system.GetMediaFile partition fileId with
                | Some mediaFile ->
                    use s3Client = createAmazonS3ServiceClient config
                    use transferUtility = new TransferUtility(s3Client)
                    try
                        use! readStream = transferUtility.OpenStreamAsync(partition, string fileId)
                        let assembled =
                            setHttpHeader "Content-Type" mediaFile.MimeType
                            >=> streamData true readStream None (Some mediaFile.UploadedOn)
                        return! assembled nxt ctx
                    with
                        | ex -> return! raise ex
                | None ->
                    return! (setStatusCode 404 >=> text "Media file not found") nxt ctx                    
            }

    let thumbnail (config: IConfiguration) (system: IMediaSystem) (size: string, partition: string, fileId: Guid) =
        fun nxt (ctx: HttpContext) ->
            task {
                match! system.GetMediaFile partition fileId with
                | Some mediaFile ->
                    use s3Client = createAmazonS3ServiceClient config
                    use transferUtility = new TransferUtility(s3Client)
                    try
                        use! readStream = transferUtility.OpenStreamAsync(partition, sprintf "%O_%s" fileId size)
                        let assembled =
                            setHttpHeader "Content-Type" mediaFile.MimeType
                            >=> streamData true readStream None (Some mediaFile.UploadedOn)
                        return! assembled nxt ctx
                    with
                        | ex -> return! raise ex
                | None ->
                    return! (setStatusCode 404 >=> text "Media file not found") nxt ctx
            }

    let getMediaFilesForEntities (system: IMediaSystem) (partition: string, entityIds: Guid list) =
        system.GetMediaFilesForEntities partition entityIds
        |> Async.map (List.groupBy (fun mediaFile -> mediaFile.EntityId))
    
    let getMediaFileById (system: IMediaSystem) (partition: string, fileId: Guid) = 
        fun (nxt) (ctx) ->
            task {
                let! mediaFileOpt = system.GetMediaFile partition fileId
                return!
                    match mediaFileOpt with
                    | Some mediaFile -> 
                        negotiate mediaFile nxt ctx
                    | None ->
                        (setStatusCode 404 >=> text "Media file not found") nxt ctx
            }

    let search (system: IMediaSystem) (partition: string) =
        fun (next : HttpFunc) (ctx : HttpContext) -> task {
            let! bound = ctx.BindJsonAsync<{| EntityIds: Guid list option |}>()
            let! mediaFiles =
                match bound.EntityIds with
                | Some entityIds -> getMediaFilesForEntities system (partition, entityIds)
                | None -> async { return [] }
            return! negotiate mediaFiles next ctx
        }
    
let mediaHandler (config) (system: IMediaSystem) =
    choose [
        //Required for (FilePond) upload
        POST >=> routeCif "/media/upload/%s/%O" post
        PATCH >=> routeCif "/media/upload/%s/%O/%O" (upload config system)
        HEAD >=> routeCif "/media/upload/%s/%O/%O" determineUploadStatus
        DELETE >=> routeCif "/media/upload/%s/%O" (delete config system)

        //Required for download
        GET >=> routeCif "/media/download/%s/%O" (download config system)
        GET >=> routeCif "/media/thumbnail/%s/%s/%O" (thumbnail config system)
        GET >=> routeCif "/media/meta/%s/%O" (getMediaFileById system)
        POST >=> routeCif "/media/meta/%s/Search" (search system)
    ]