module Server.Authentication.Storage

open System
open Server.Blueprint.Data.Authentication
open Server.AppSettings
open Npgsql.FSharp
open Server.PostgreSQL
open Shared.Read
open Shared.ConstrainedTypes
open Shared.Trial
open Shared.Trial.Control

type EncryptedUpdateTwoFacAuthentication = 
    {
        UserId: Guid
        UseTwoFac: bool
        TwoFacSecret: byte[]
        RecoveryCodeHashes: byte[] list
    }
    static member EncryptRecoveryCode (recoveryCodePepper: string) (recoveryCode: string) =
        Encryption.hashPassword recoveryCodePepper recoveryCode
    static member Encrypt (twoFacPassword: string) (recoveryCodePepper: string) (updateTwoFacAuthentication: UpdateTwoFacAuthentication) = {
        UserId = updateTwoFacAuthentication.UserId
        UseTwoFac = updateTwoFacAuthentication.UseTwoFac
        TwoFacSecret = Encryption.encryptString (updateTwoFacAuthentication.TwoFacSecret, twoFacPassword)
        RecoveryCodeHashes = 
            updateTwoFacAuthentication.RecoveryCodes
            |> List.map (EncryptedUpdateTwoFacAuthentication.EncryptRecoveryCode recoveryCodePepper)
    }

type ValidatedUserInput = 
    {
        UserId: Guid
        DisplayName: String255
        EmailAddress: String255
        Roles: Role list
        PreferredLanguageCode: String16
        PasswordHash: byte []
    }
    static member Validate (passwordPepper) (user: UserInput) =        
        trial {
            from displayName in String255.Of (nameof user.DisplayName) user.DisplayName
            also emailAddress in String255.Of (nameof user.EmailAddress) user.EmailAddress
            also preferredLanguageCode in String16.Of (nameof user.PreferredLanguageCode) user.PreferredLanguageCode
            yield {
                UserId = user.UserId
                DisplayName = displayName
                EmailAddress = emailAddress
                Roles = user.Roles
                PreferredLanguageCode = preferredLanguageCode
                PasswordHash = Encryption.hashPassword passwordPepper user.Password
            }
        }
        |> Trial.toResult

type IAuthenticationStorage =
    abstract DisableAccount: userId: Guid -> Async<int>
    abstract EnableAccount: userId: Guid -> Async<int>
    abstract UpdateTwoFacAuthentication: update: EncryptedUpdateTwoFacAuthentication -> Async<int>
    abstract RemoveUsedRecoveryCode: userId: Guid * recoveryCodeHash: byte[] -> Async<int>
    abstract UpdateRecoveryCodes: userId: Guid * codes: byte[] list -> Async<int>
    abstract AddFailedTwoFacAttempt: FailedTwoFacAttempt -> Async<unit>
    abstract AddUser: ValidatedUserInput -> Async<unit>

let disableAccount conn userId =
    Sql.connect conn
    |> Sql.query "UPDATE [Users] SET IsActive = FALSE WHERE UserId = @UserId"
    |> Sql.parameters [ "@UserId", Sql.uuid userId ]
    |> Sql.writeAsync

let enableAccount conn userId =
    Sql.connect conn
    |> Sql.query "UPDATE [Users] SET IsActive = TRUE WHERE UserId = @UserId"
    |> Sql.parameters [ "@UserId", Sql.uuid userId ]
    |> Sql.writeAsync

let updateTwoFacAuthentication conn (update: EncryptedUpdateTwoFacAuthentication) =
    Sql.connect conn
    |> Sql.writeBatchAsync [
        yield
            "UPDATE [Users] SET UseTwoFac = @UseTwoFac AND TwoFacSecret = @TwoFacSecret WHERE UserId = @UserId", [
                "@UseTwoFac", Sql.bool update.UseTwoFac
                "@TwoFacSecret", Sql.bytea update.TwoFacSecret
                "@UserId", Sql.uuid update.UserId
            ]
        yield!
            update.RecoveryCodeHashes 
            |> List.map (fun recoveryCodeHash ->
                "INSERT INTO [RecoveryCodes] (UserId, RecoveryCodeHash) VALUES (@UserId, @RecoveryCodeHash)", [
                    "@UserId", Sql.uuid update.UserId
                    "@RecoveryCodeHash", Sql.bytea recoveryCodeHash
                ])
    ]
    |> Async.map List.sum

let removeUsedRecoveryCode conn (userId, recoveryCodeHash) =
    Sql.connect conn
    |> Sql.query "DELETE FROM [RecoveryCodes] WHERE UserId = @UserId AND RecoveryCodeHash = @RecoveryCodeHash"
    |> Sql.parameters [ "@UserId", Sql.uuid userId; "@RecoveryCodeHash", Sql.bytea recoveryCodeHash ]
    |> Sql.writeAsync

let setRecoveryCodeHashes conn (userId, recoveryCodeHashes) =
    Sql.connect conn
    |> Sql.writeBatchAsync [
        yield 
            "DELETE FROM [RecoveryCodes] WHERE UserId = @UserId", [ "@UserId", Sql.uuid userId ]

        yield!
            recoveryCodeHashes 
            |> List.map (fun recoveryCodeHash ->
                "INSERT INTO [RecoveryCodes] (UserId, RecoveryCodeHash) VALUES (@UserId, @RecoveryCodeHash)", [
                    "@UserId", Sql.uuid userId
                    "@RecoveryCodeHash", Sql.bytea recoveryCodeHash
                ])
    ]
    |> Async.map (List.skip 1 >> List.sum)

let addFailedTwoFacAttempt conn (attempt: FailedTwoFacAttempt) =
    Sql.connect conn
    |> Sql.query "INSERT INTO [FailedTwoFacEvents] (UserId, TimeStamp) VALUES (@UserId, @TimeStamp)"
    |> Sql.parameters [
        "@UserId", Sql.uuid attempt.UserId
        "@TimeStamp", Sql.timestamp attempt.Timestamp.UtcDateTime
    ]
    |> Sql.writeAsync
    |> Async.Ignore

let addUser conn (user: ValidatedUserInput) =
    Sql.connect conn
    |> Sql.writeBatchAsync [        
        yield
            """
                INSERT INTO Users (
                    UserId,
                    DisplayName,
                    EmailAddress,
                    PreferredLanguageCode,
                    PasswordHash
                )
                VALUES (
                    @UserId,
                    @DisplayName,
                    @EmailAddress,
                    @PreferredLanguageCode,
                    @PasswordHash
                )
            """, [
                "@UserId", Sql.uuid user.UserId
                "@DisplayName", Sql.string (string user.DisplayName)
                "@EmailAddress", Sql.string (string user.EmailAddress)
                "@PreferredLanguageCode", Sql.string (string user.PreferredLanguageCode)
                "@PasswordHash", Sql.bytea user.PasswordHash
            ]
        yield
            "DELETE FROM UserRoles WHERE UserId = @UserId", [
                "@UserId", Sql.uuid user.UserId
            ]
        yield! 
            user.Roles 
            |> List.collect (fun role ->
                match role with
                | UserRole bIds ->
                    bIds |> List.map (fun bId -> nameof UserRole, Some bId, None)
                | SyndicRole bIds ->
                    bIds |> List.map (fun bId -> nameof SyndicRole, Some bId, None)
                | ProfessionalSyndicRole (orgId, _) ->
                    [ nameof ProfessionalSyndicRole, None, Some orgId ]
                | SysAdminRole ->
                    [ nameof SysAdmin, None, None ]
            )
            |> List.map (fun (role, buildingId, orgId) ->
                """
                    INSERT INTO UserRoles 
                        (UserId, Role, BuildingId, OrganizationId)
                    VALUES
                        (@UserId, @Role, @BuildingId, @OrganizationId)
                """, [
                    "@UserId", Sql.uuid user.UserId
                    "@Role", Sql.string role
                    "@BuildingId", Sql.uuidOrNone buildingId
                    "@OrganizationId", Sql.uuidOrNone orgId
                ]
            )
    ]
    |> Async.Ignore

let makeStorage (settings: AppSettings) = 
    let conn = settings.Database.ConnectionString

    {
        new IAuthenticationStorage with
            member _.DisableAccount userId = disableAccount conn userId
            member _.EnableAccount userId = enableAccount conn userId
            member _.UpdateTwoFacAuthentication update = updateTwoFacAuthentication conn update
            member _.RemoveUsedRecoveryCode (userId, recoveryCodeHash) = removeUsedRecoveryCode conn (userId, recoveryCodeHash)
            member _.UpdateRecoveryCodes (userId, codes) = setRecoveryCodeHashes conn (userId, codes)
            member _.AddFailedTwoFacAttempt (attempt) = addFailedTwoFacAttempt conn (attempt)
            member _.AddUser user = addUser conn user
    }