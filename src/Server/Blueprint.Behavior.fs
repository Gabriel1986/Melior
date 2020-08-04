module Server.Blueprint.Behavior

open System

module Authentication =
    open Giraffe
    open Server.Blueprint.Data.Authentication
    open Shared.Read
    open System.Security.Claims

    [<NoComparison; NoEquality>]
    type IAuthenticationSystem =
        //Commands
        abstract ValidateTwoFactorPIN: userId: Guid * validationCode: string -> Async<bool>
        abstract UpdateTwoFacAuthentication: UpdateTwoFacAuthentication -> Async<unit>
        abstract RemoveUsedRecoveryCode: email: string * code: string -> Async<unit>
        abstract GenerateNewRecoveryCodes: email: string -> Async<string list>
        abstract DisableAccount: userId: Guid -> Async<unit>
        abstract EnableAccount: userId: Guid -> Async<unit>
        abstract AddFailedTwoFacAttempt: FailedTwoFacAttempt -> Async<unit>

        abstract GenerateChangePasswordToken: claims: Claim list * expiresAfter: TimeSpan -> string
        abstract ValidateChangePasswordToken: token: string -> ClaimsPrincipal option
        abstract GenerateUsernamePasswordToken: claims: Claim list * expiresAfter: TimeSpan -> string
        abstract ValidateUsernamePasswordToken: token: string -> ClaimsPrincipal option

        //Queries
        abstract GetTwoFacPassword: userId: Guid -> Async<string option>
        abstract AuthenticateUser: emailAddress: string * password: string -> Async<Result<User, AuthenticationError>>
        abstract FindUserByEmailAddress: email: string -> Async<User option>
        abstract GetUser: userId: Guid -> Async<User option>
        abstract GetNbFailedTwoFacAttempts: userId: Guid * after: DateTimeOffset -> Async<int>
        abstract HttpHandler: HttpHandler

[<NoComparison; NoEquality>]
type IEnv =
    abstract AuthenticationSystem: Authentication.IAuthenticationSystem