module Server.Authentication.Configuration

    open Microsoft.AspNetCore.Http
    open Microsoft.AspNetCore.Authentication.Cookies
    open Microsoft.Extensions.DependencyInjection
    open System.IdentityModel.Tokens.Jwt

    let addAuthenticationServices (services: IServiceCollection) =
        //How is this still a thing anno 2020?
        JwtSecurityTokenHandler.DefaultInboundClaimTypeMap.Clear();
        JwtSecurityTokenHandler.DefaultOutboundClaimTypeMap.Clear();

        services
            .AddAuthentication(fun options ->
                options.DefaultScheme <- CookieAuthenticationDefaults.AuthenticationScheme
                options.RequireAuthenticatedSignIn <- true
            )
            .AddCookie(CookieAuthenticationDefaults.AuthenticationScheme, fun options ->
                options.Cookie.SameSite <- SameSiteMode.Strict
                options.Cookie.HttpOnly <- true
                options.AccessDeniedPath <- PathString.FromUriComponent("/authentication/accessDenied")
                options.LoginPath <- PathString.FromUriComponent("/authentication/login")
                options.LogoutPath <- PathString.FromUriComponent("/authentication/logout")
            )
        |> ignore