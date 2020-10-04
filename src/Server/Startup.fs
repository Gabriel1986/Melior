namespace Server

open System
open System.Threading.Tasks
open Giraffe
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.HttpOverrides
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Primitives
open Thoth.Json.Net
open Thoth.Json.Giraffe
open Serilog

open Server.ProfessionalSyndics.ProfessionalSyndicCache
open Server.AppSettings
open Server.Blueprint.Behavior.ProfessionalSyndics

type Startup private () =
    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    // This method gets called by the runtime. Use this method to add services to the container.
    member this.ConfigureServices(services: IServiceCollection) =
        let appSettings = this.Configuration.Get<AppSettings>()

        services
            .AddGiraffe()
            .AddAntiforgery()
            .AddSingleton<Serialization.Json.IJsonSerializer>(
                ThothSerializer (caseStrategy=Thoth.Json.Net.CaseStrategy.PascalCase, extra=Extra.empty, skipNullField=true)
            )
            .AddSingleton<IProfessionalSyndicCache>(
                new ProfessionalSyndicCache(appSettings.Database.Connection)
            )
            |> ignore


        Authentication.Configuration.addAuthenticationServices (services)
        ()



    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member this.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) =
        let appSettings = this.Configuration.Get<AppSettings>()
        Migrations.run Log.Logger appSettings.Database.Connection
        Seedings.run Log.Logger this.Configuration

        let handleErrors: ErrorHandler =
            fun ex logger ->
                printf "Error: %O" ex
                logger.LogError (EventId(), ex, "An unhandled exception has occurred while executing the request.")
                clearResponse >=> ServerErrors.INTERNAL_ERROR ex

        if (env.IsDevelopment()) then
            app.UseDeveloperExceptionPage() |> ignore


        let rewriteWithForwardedHeaders (ctx: HttpContext) (nxt: Func<Task>) =
            let (ok, proto) = ctx.Request.Headers.TryGetValue("X-FORWARDED-PROTO")
            if ok && (StringValues "https") = proto then
                ctx.Request.Scheme <- "https"
            else
                let (ok, schema) = ctx.Request.Headers.TryGetValue("X-FORWARDED-SCHEMA")
                if ok && (StringValues "https") = schema then ctx.Request.Scheme <- "https"

            let (ok, host) = ctx.Request.Headers.TryGetValue("X-FORWARDED-HOST")
            if ok && 0 < host.Count then ctx.Request.Host <- HostString host.[0]

            let (ok, path) = ctx.Request.Headers.TryGetValue("X-FORWARDED-PATH")
            if ok && 0 < path.Count && not (String.IsNullOrWhiteSpace path.[0]) then
                let path' = sprintf "/%s" (path.[0].TrimStart('/'))
                ctx.Request.PathBase <- PathString path'

            nxt.Invoke()

        app
            .UseDefaultFiles()
            .UseStaticFiles()
            .UseForwardedHeaders(new ForwardedHeadersOptions(ForwardedHeaders=(ForwardedHeaders.XForwardedFor|||ForwardedHeaders.XForwardedProto)))
            .Use(rewriteWithForwardedHeaders) //NOTE needed for reverse-proxy support
            .UseAuthentication()
            .UseGiraffeErrorHandler(handleErrors)
            .UseGiraffe(Application.build this.Configuration)

    member val Configuration : IConfiguration = null with get, set
