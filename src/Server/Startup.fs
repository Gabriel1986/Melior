namespace Server

open Giraffe
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Hosting
open Thoth.Json.Net
open Thoth.Json.Giraffe
open Serilog

open Server.AppSettings

type Startup private () =
    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    // This method gets called by the runtime. Use this method to add services to the container.
    member this.ConfigureServices(services: IServiceCollection) =
        services
            .AddGiraffe()
            .AddSingleton<Serialization.Json.IJsonSerializer>(
                ThothSerializer (caseStrategy=Thoth.Json.Net.CaseStrategy.PascalCase, extra=Extra.empty, skipNullField=true)
            )
            |> ignore

        Authentication.Configuration.addAuthenticationServices (services)
        ()

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member this.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) =
        let appSettings = this.Configuration.Get<AppSettings>()
        Migrations.run Log.Logger appSettings.Database.ConnectionString 

        let handleErrors: ErrorHandler =
            fun ex logger ->
                printf "Error: %O" ex
                logger.LogError (EventId(), ex, "An unhandled exception has occurred while executing the request.")
                clearResponse >=> ServerErrors.INTERNAL_ERROR ex

        if (env.IsDevelopment()) then
            app.UseDeveloperExceptionPage() |> ignore

        app
            .UseHttpsRedirection()
            .UseDefaultFiles()
            .UseStaticFiles()
            .UseAuthentication()
            .UseGiraffeErrorHandler(handleErrors)
            .UseGiraffe(Application.build this.Configuration)

    member val Configuration : IConfiguration = null with get, set
