module Server.Blueprint.Data

open System
open Shared.Read

module SeedData =
    type FinancialCategorySeedRow = {
        Code: string
        Description: string
    }

    type DistributionKeySeedRow = {
        DistributionKeyId: Guid
        BuildingId: Guid option
        Name: string
        DistributionType: DistributionType
        LotsOrLotTypes: LotsOrLotTypes
        IncludeGroundFloor: bool
    }


module Authentication =
    type UpdateTwoFacAuthentication = {
        UserId: Guid
        UseTwoFac: bool
        TwoFacSecret: string
        RecoveryCodes: string list
    }

    type FailedTwoFacAttempt = {
        UserId: Guid
        Timestamp: DateTimeOffset
    }

    type AuthenticationError =
        | PasswordNotValid
        | UserNotFound

    type UserInput = {
        UserId: Guid
        DisplayName: string
        EmailAddress: string
        Roles: Role list
        PreferredLanguageCode: string
        Password: string
    }

    type CreateUserError =
        | ValidationError of (string * string) list
        | Unauthorized