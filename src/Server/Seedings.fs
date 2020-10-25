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

let private seedFinancialCategories (logger: ILogger) (environment: IEnv) (migratedFrom: int64, migratedTo: int64) =
    if (migratedFrom < 10L && migratedTo >= 10L) then
        logger.Debug("Seeding financial categories")
        async {
            try
                let! financialCategories = FinancialCategories.readPredefined ()
                do! environment.FinancialSystem.SeedFinancialCategories financialCategories
                logger.Debug("Finished seeding financial categories")
            with error ->
                logger.Error(error, "Something went wrong while seeding the financial categories")
        }
        |> Async.RunSynchronously
    else
        ()

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
    else
        ()

let run (logger: ILogger) (config: IConfiguration) (migratedFrom: int64, migratedTo: int64): unit =
    logger.Debug(sprintf "Running seeding from version %i to version %i" migratedFrom migratedTo)
    let appSettings = config.Get<AppSettings>()
    let environment = Application.createEnvironment config

    seedAdmin logger appSettings environment
    seedFinancialCategories logger environment (migratedFrom, migratedTo)
    seedDistributionKeys logger environment (migratedFrom, migratedTo)
    logger.Debug("Seeding completed")
