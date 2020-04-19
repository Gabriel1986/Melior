namespace Server

open Giraffe
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.HttpOverrides
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.Authentication.JwtBearer
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Hosting
open Microsoft.IdentityModel.Tokens
open Thoth.Json.Net
open Thoth.Json.Giraffe

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
            ) |> ignore

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member this.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) =
        let handleErrors: ErrorHandler =
            fun ex logger ->
                logger.LogError (EventId(), ex, "An unhandled exception has occurred while executing the request.")
                clearResponse >=> ServerErrors.INTERNAL_ERROR ex

        if (env.IsDevelopment()) then
            app.UseDeveloperExceptionPage() |> ignore

        let build = Application.build ()

        app
            .UseHttpsRedirection()
            .UseDefaultFiles()
            .UseStaticFiles()
            //.UseAuthentication()
            .UseGiraffeErrorHandler(handleErrors)
            .UseGiraffe(Application.build ())

    member val Configuration : IConfiguration = null with get, set
