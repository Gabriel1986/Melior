namespace Server

open System.IO
open Serilog
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open System

module Program =
    let exitCode = 0

    let CreateHostBuilder (args: string []) =
        let contentPath =
            Directory.GetCurrentDirectory()
            |> sprintf "%s/../Client/public"
            |> Path.GetFullPath


        let config =
            ConfigurationBuilder()
                .AddInMemoryCollection(dict [])
                .SetBasePath(AppDomain.CurrentDomain.BaseDirectory)
                .AddJsonFile("appsettings.json", optional = false)
                .AddEnvironmentVariables()
                .AddCommandLine(args)
                .Build()

        Log.Logger <- LoggerConfiguration()
                            .ReadFrom.Configuration(config)
                            .CreateLogger()

        do
            config.AsEnumerable (false)
            |> Seq.iter (fun kv -> Log.Logger.Debug(sprintf "Key: %s - Value: %s" kv.Key kv.Value))

        WebHostBuilder()
            .UseConfiguration(config)
            .UseKestrel(fun _ options -> options.Configure(config.GetSection("Kestrel")) |> ignore)
            .UseWebRoot(contentPath)
            .UseContentRoot(contentPath)
            .UseStartup<Startup>()

    [<EntryPoint>]
    let main args =
        CreateHostBuilder(args).Build().Run()

        exitCode
