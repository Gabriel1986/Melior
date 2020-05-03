namespace Server

open System.IO
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Serilog
open Serilog.Events
open Serilog.Sinks.SystemConsole.Themes

module Program =
    let exitCode = 0

    let CreateHostBuilder args =
        let contentPath =
            Directory.GetCurrentDirectory()
            |> sprintf "%s/../Client/public"
            |> Path.GetFullPath

        Host.CreateDefaultBuilder(args)
            .UseContentRoot(contentPath)
            .ConfigureWebHostDefaults(fun webBuilder ->
                webBuilder
                    .ConfigureKestrel(fun options ->  options.ListenLocalhost(8091))
                    .UseStartup<Startup>()
                    .UseWebRoot(contentPath)
                |> ignore
                    //TODO: read config from configuration file.
                    //.UseSerilog(configureLogger = (fun (context: WebHostBuilderContext)  (configuration: LoggerConfiguration) ->
                    //    configuration
                    //        .MinimumLevel.Debug()
                    //        .MinimumLevel.Override("Microsoft", LogEventLevel.Warning)
                    //        .MinimumLevel.Override("System", LogEventLevel.Warning)
                    //        .MinimumLevel.Override("Microsoft.AspNetCore.Authentication", LogEventLevel.Information)
                    //        .Enrich.FromLogContext()
                    //        .WriteTo.File(@"server_log.txt")
                    //        .WriteTo.Console(
                    //            outputTemplate = "[{Timestamp:HH:mm:ss} {Level}] {SourceContext}{NewLine}{Message:lj}{NewLine}{Exception}{NewLine}",
                    //            theme = AnsiConsoleTheme.Literate
                    //        )
                    //    |> ignore
            )

    [<EntryPoint>]
    let main args =
        CreateHostBuilder(args).Build().Run()

        exitCode
