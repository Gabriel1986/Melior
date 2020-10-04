module Server.Seedings

open System
open Microsoft.Extensions.Configuration
open AppSettings
open Shared.Read
open Server.Blueprint.Data.Authentication

let run (logger: Serilog.ILogger) (config: IConfiguration): unit =
    logger.Debug("Running seeding")
    let appSettings = config.Get<AppSettings>()
    let environment = Application.createEnvironment config

    printfn "Creating %i sysadmins" appSettings.Authentication.SysAdmins.Length
    appSettings.Authentication.SysAdmins
    |> Array.iter (fun sysAdmin ->
        match (environment.AuthenticationSystem.UserWithEmailAddressExists sysAdmin.EmailAddress |> Async.RunSynchronously) with
        | true ->
            do ()
        | false ->
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
                do ()
            | Error error ->
                logger.Error(sprintf "Could not add system user '%s': '%A'" sysAdmin.EmailAddress error)
    )