module Server.Seedings

open System
open Microsoft.Extensions.Configuration
open Serilog
open Npgsql.FSharp
open AppSettings
open Shared.Read
open Server.Blueprint.Data.Authentication
open Server.Blueprint.Behavior
open Server.SeedData
open Server.Library
open Shared.MediaLibrary

//This seeding should always occur, in case the main admin accidently removes its admin role :D
let private seedAdmin (logger: Serilog.ILogger) (appSettings: AppSettings) (environment: IEnv) =
    appSettings.Authentication.SysAdmins
    |> Array.iter (fun sysAdmin ->
        match (environment.AuthenticationSystem.UserWithEmailAddressExists sysAdmin.EmailAddress |> Async.RunSynchronously) with
        | true ->
            do ()
        | false ->
            logger.Debug("Seeding default system admin")
            let newUser: UserInput = {
                UserId = Guid.NewGuid()
                EmailAddress = sysAdmin.EmailAddress
                DisplayName = sysAdmin.DisplayName
                Roles = [ SysAdminRole ]
                PreferredLanguageCode = sysAdmin.PreferredLanguageCode
                Password = sysAdmin.Password
            }
            match environment.AuthenticationSystem.AddUser newUser |> Async.RunSynchronously with
            | Ok _ -> 
                logger.Debug("Default system admin was seeded")
                do ()
            | Error error ->
                logger.Error(sprintf "Could not add system user '%s': '%A'" sysAdmin.EmailAddress error)
    )

let private seedDistributionKeys (logger: ILogger) (environment: IEnv) (migratedFrom: int64, migratedTo: int64) =
    if (migratedFrom < 10L && migratedTo >= 10L) then
        logger.Debug("Seeding distribution keys")
        async {
            try
                let distributionKeys = DistributionKeys.predefinedDistributionKeys
                do! environment.FinancialSystem.SeedDistributionKeys distributionKeys
                logger.Debug("Finished seeding distribution keys")
            with error ->
                logger.Error(error, "Something went wrong while seeding the financial categories")
        }
        |> Async.RunSynchronously
    if (migratedFrom < 21L && migratedTo >= 21L) then
        logger.Debug("Seeding OGM References")
        async {
            do! environment.LotSystem.GenerateOGMReferences ()
        }
        |> Async.RunSynchronously
    else
        ()

let private copyS3ObjectToNewLocation (logger: ILogger) (client: Amazon.S3.IAmazonS3) (size: string option, mediaFile: MediaFile) = async {
    try
        logger.Debug(sprintf "Copying media file '%A' to new location in S3" mediaFile.FileId)
        do!
            client.CopyObjectAsync("meliordigital", sprintf "%s/%O" mediaFile.Partition mediaFile.FileId, "meliordigital", Media.HttpHandler.s3BucketRoute (mediaFile, size))
            |> Async.AwaitTask
            |> Async.Ignore
        logger.Debug(sprintf "Media file: '%A' was copied to new location in S3" mediaFile.FileId)
    with error ->
        logger.Error(error, sprintf "Something went wrong while copying media file '%A'" mediaFile.FileId)
}

let private copyMediaFilesToNewLocation (logger: ILogger) (config: IConfiguration) (environment: IEnv) (migratedFrom: int64, migratedTo: int64) =   
    if (migratedFrom < 16L && migratedTo >= 16L) then
        logger.Debug("Copying media files from the old location to the new location")
        async {
            let copyS3ObjectToNewLocation = copyS3ObjectToNewLocation logger (Media.HttpHandler.createAmazonS3ServiceClient config)
            let! allMediaFiles = environment.MediaSystem.GetAllMediaFiles ()
            for mediaFile in allMediaFiles do
                do! copyS3ObjectToNewLocation (None, mediaFile)
                if mediaFile.IsImage() then
                    do! copyS3ObjectToNewLocation (Some "large", mediaFile)
                    do! copyS3ObjectToNewLocation (Some "small", mediaFile)
        }
        |> Async.RunSynchronously
    else
        ()

let run (logger: ILogger) (config: IConfiguration) (migratedFrom: int64, migratedTo: int64): unit =
    logger.Debug(sprintf "Running seeding from version %i to version %i" migratedFrom migratedTo)
    let appSettings = config.Get<AppSettings>()
    let environment = Application.createEnvironment config

    seedAdmin logger appSettings environment
    seedDistributionKeys logger environment (migratedFrom, migratedTo)
    copyMediaFilesToNewLocation logger config environment (migratedFrom, migratedTo)
    logger.Debug("Seeding completed")
