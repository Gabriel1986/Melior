module Server.LibraryExtensions

open System
open System.Security.Claims
open System.IdentityModel.Tokens.Jwt
open Microsoft.AspNetCore.Http
open Thoth.Json.Net
open Shared.Read
open Server.Library
open Server.Blueprint.Behavior.ProfessionalSyndics

module User =
    let private roleEncoder = Thoth.Json.Net.Encode.Auto.generateEncoderCached<Role>()
    let private roleToJson role = 
        match role with
        | UserRole _
        | SyndicRole _
        | SysAdminRole ->
            Encode.toString 0 (roleEncoder role)
        | ProfessionalSyndicRole (orgId, _buildingIds) ->
            Encode.toString 0 (roleEncoder (ProfessionalSyndicRole (orgId, [])))
    
    let private roleDecoder = Thoth.Json.Net.Decode.Auto.generateDecoderCached<Role>()
    let private jsonToRole str (proSyndicCache: IProfessionalSyndicCache) =
        match Decode.fromString roleDecoder str with
        | Ok (ProfessionalSyndicRole (orgId, _)) -> Ok (ProfessionalSyndicRole (orgId, proSyndicCache.GetBuildingIdsForProfessionalSyndic orgId))
        | any -> any

    let [<Literal>] private RoleClaimType = "role"
    let [<Literal>] private LocaleClaimType = "locale"
    let [<Literal>] private UseTwoFacClaimType = "use2fac"

    let OfContext (context: HttpContext): User = 
        let claimsPrincipal = context.User
        let professionalSyndicCache = context.GetService<IProfessionalSyndicCache>()
        {
            UserId = Guid.Parse (claimsPrincipal.FindFirstValue(JwtRegisteredClaimNames.Sub))
            EmailAddress = claimsPrincipal.FindFirstValue(JwtRegisteredClaimNames.Email)
            DisplayName = claimsPrincipal.FindFirstValue(JwtRegisteredClaimNames.GivenName)
            Roles = 
                claimsPrincipal.FindAll(RoleClaimType) 
                |> Seq.map (fun c -> jsonToRole c.Value professionalSyndicCache)
                |> Seq.choose (fun r -> r |> Option.fromResult) 
                |> List.ofSeq
            PreferredLanguageCode = claimsPrincipal.FindFirstValue(LocaleClaimType)
            UseTwoFac = claimsPrincipal.FindFirstValue(UseTwoFacClaimType) = "1"
        }

    let private toClaimsIdentity (authSchema: string) claims = ClaimsIdentity(claims, authSchema)
    let private toClaimsPrincipal (identity: ClaimsIdentity) = ClaimsPrincipal(identity)

    let ToClaimsPrincipal (authSchema: string) (user: User): ClaimsPrincipal =
        [
            yield new Claim(JwtRegisteredClaimNames.Sub, string user.UserId)
            yield new Claim(JwtRegisteredClaimNames.Email, user.EmailAddress)
            yield new Claim(JwtRegisteredClaimNames.GivenName, user.DisplayName)
            yield! user.Roles |> List.map (fun role -> new Claim(RoleClaimType, roleToJson role))
            yield new Claim(LocaleClaimType, user.PreferredLanguageCode)
            yield new Claim(UseTwoFacClaimType, if user.UseTwoFac then "1" else "0")
        ]
        |> toClaimsIdentity authSchema
        |> toClaimsPrincipal

type Message<'T> with
    member me.CurrentUser =
        User.OfContext me.Context
    member me.ProfessionalSyndicCache =
        me.Context.GetService<IProfessionalSyndicCache>()