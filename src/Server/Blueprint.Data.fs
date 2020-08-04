module Server.Blueprint.Data

open System

module Authentication =
    type UpdateTwoFacAuthentication = {
        UserId: Guid
        UseTwoFac: bool
        TwoFacSecret: string
        EmailAddress: string
        RecoveryCodes: string list
    }

    type FailedTwoFacAttempt = {
        UserId: Guid
        Timestamp: DateTimeOffset
    }

    type AuthenticationError =
        | PasswordNotValid
        | UserNotFound