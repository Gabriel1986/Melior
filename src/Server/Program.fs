namespace Server

open System.IO
open Serilog
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open System
open Amazon.Runtime

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

        let awsConfig = config.GetAWSOptions()
        awsConfig.Credentials <- new EnvironmentVariablesAWSCredentials()
        let awsCredentials = awsConfig.Credentials.GetCredentials()

        Log.Logger <-
            #if DEBUG
            (new LoggerConfiguration())
                .ReadFrom.Configuration(config)
                .CreateLogger()
            #else
            (new LoggerConfiguration())
                .WriteTo
                .AmazonS3(
                    path = "SyndicusAssistent.txt", 
                    bucketName = "meliordigital",
                    endpoint = Amazon.RegionEndpoint.EUCentral1,
                    awsAccessKeyId = awsCredentials.AccessKey,
                    awsSecretAccessKey = awsCredentials.SecretKey,                    
                    fileSizeLimitBytes = 1024L * 20L,
                    bucketPath = "Logs",
                    restrictedToMinimumLevel = LogEventLevel.Warning,
                    autoUploadEvents = false)
                .CreateLogger()
            #endif

        WebHostBuilder()
            .UseConfiguration(config)
            .UseSerilog()
            .UseKestrel(fun _ options -> options.Configure(config.GetSection("Kestrel")) |> ignore)
            .UseWebRoot(contentPath)
            .UseContentRoot(contentPath)
            .UseStartup<Startup>()

    [<EntryPoint>]
    let main args =
        CreateHostBuilder(args).Build().Run()

        exitCode
