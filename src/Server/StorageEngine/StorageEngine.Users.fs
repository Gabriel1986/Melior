module Server.StorageEngine.UserStorage

open System
open Npgsql.FSharp
open Shared.Read
open Shared.Write
open Server.Library
open Server.PostgreSQL
open Server.Blueprint.Data.Storage
open Server.Blueprint.Data.Authentication

let paramsFor (validated: ValidatedUser) = [
    "@UserId", Sql.uuid validated.UserId
    "@DisplayName", Sql.string (string validated.DisplayName)
    "@EmailAddress", Sql.string (string validated.EmailAddress)
    "@PreferredLanguageCode", Sql.string (string validated.PreferredLanguageCode)
    "@UseTwoFac", Sql.bool validated.UseTwoFac
]

let private setRecoveryCodeHashesQuery (userId, recoveryCodeHashes) =
    [
        yield
            "DELETE FROM RecoveryCodes WHERE UserId = @UserId",
            [[ "@UserId", Sql.uuid userId ]]
        yield
            "INSERT INTO RecoveryCodes (UserId, RecoveryCodeHash) VALUES (@UserId, @RecoveryCodeHash)",
            recoveryCodeHashes
            |> List.map (fun recoveryCodeHash -> [
                "@UserId", Sql.uuid userId
                "@RecoveryCodeHash", Sql.bytea recoveryCodeHash
            ])    
    ]

let private setUserRoles (userId: Guid, roles: Role list) = [
    "DELETE FROM UserRoles WHERE UserId = @UserId", [[
        "@UserId", Sql.uuid userId
    ]]

    """
        INSERT INTO UserRoles 
            (UserId, Role, BuildingId, OrganizationId)
        VALUES
            (@UserId, @Role, @BuildingId, @OrganizationId)
    """, 
        roles 
        |> List.collect (fun role ->
            match role with
            | UserRole bIds ->
                bIds |> List.map (fun bId -> nameof UserRole, Some bId, None)
            | SyndicRole bIds ->
                bIds |> List.map (fun bId -> nameof SyndicRole, Some bId, None)
            | ProfessionalSyndicRole (orgId, _) ->
                [ nameof ProfessionalSyndicRole, None, Some orgId ]
            | SysAdminRole ->
                [ nameof SysAdminRole, None, None ]
        )
        |> List.map (fun (role, buildingId, orgId) ->
            [
                "@UserId", Sql.uuid userId
                "@Role", Sql.string role
                "@BuildingId", Sql.uuidOrNone buildingId
                "@OrganizationId", Sql.uuidOrNone orgId
            ]
        )
]

let transformEventToSql (msg: Message<UserEvent>) =
    match msg.Payload with
    | UserEvent.TwoFactorAuthenticationWasUpdated update ->
        [
            yield
                "UPDATE Users SET UseTwoFac = @UseTwoFac, TwoFacSecret = @TwoFacSecret WHERE UserId = @UserId", [[
                    "@UseTwoFac", Sql.bool update.UseTwoFac
                    "@TwoFacSecret", Sql.bytea update.TwoFacSecret
                    "@UserId", Sql.uuid update.UserId
                ]]
            yield! setRecoveryCodeHashesQuery (update.UserId, update.RecoveryCodeHashes)
        ]
    //Used by the system
    | UserEvent.UserWasAdded validated ->
        [
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
            """, [[
                "@UserId", Sql.uuid validated.UserId
                "@DisplayName", Sql.string (string validated.DisplayName)
                "@EmailAddress", Sql.string (string validated.EmailAddress)
                "@PreferredLanguageCode", Sql.string (string validated.PreferredLanguageCode)
                "@PasswordHash", Sql.bytea validated.PasswordHash
            ]]

            yield! setUserRoles (validated.UserId, validated.Roles)
        ]
    //Used by the users of the system
    | UserEvent.UserEvent event ->
        match event with
        | CUDEvent.Created validated ->
            [
                yield
                    """
                        INSERT INTO Users (
                            UserId,
                            DisplayName,
                            EmailAddress,
                            PreferredLanguageCode
                        )
                        VALUES (
                            @UserId,
                            @DisplayName,
                            @EmailAddress,
                            @PreferredLanguageCode
                        )
                    """, [ paramsFor validated ]

                yield! setUserRoles (validated.UserId, validated.Roles)
            ]
        | CUDEvent.Updated validated ->
            [
                yield
                    """
                        UPDATE Users 
                        SET 
                            DisplayName = @DisplayName,
                            EmailAddress = @EmailAddress,
                            PreferredLanguageCode = @PreferredLanguageCode,
                            UseTwoFac = @UseTwoFac
                        WHERE
                            UserId = @UserId
                    """, [ paramsFor validated ]

                yield! setUserRoles (validated.UserId, validated.Roles)
            ]
        | CUDEvent.Deleted userId ->
            [
                "UPDATE Users SET IsActive = FALSE WHERE @UserId = UserId", [[ "@UserId", Sql.uuid userId ]]
            ]
    | UserEvent.FailedTwoFacAttemptWasAdded attempt ->
        [
            "INSERT INTO FailedTwoFacEvents (UserId, TimeStamp) VALUES (@UserId, @TimeStamp)", [[
                "@UserId", Sql.uuid attempt.UserId
                "@TimeStamp", Sql.timestamp attempt.Timestamp.UtcDateTime
            ]]
        ]
    | UserEvent.PasswordWasChanged (userId: Guid, passwordHash: byte []) ->
        [
            "Update Users set PasswordHash = @PasswordHash where UserId = @UserId", [[
                "@UserId", Sql.uuid userId
                "@PasswordHash", Sql.bytea passwordHash
            ]]
        ]
    | UserEvent.RecoveryCodeWasUsed (userId: Guid, recoveryCodeHash: byte []) ->
        [
            "DELETE FROM RecoveryCodes WHERE UserId = @UserId AND RecoveryCodeHash = @RecoveryCodeHash"
            , [[ "@UserId", Sql.uuid userId; "@RecoveryCodeHash", Sql.bytea recoveryCodeHash  ]]
        ]
    | UserEvent.RecoveryCodesWereUpdated (userId: Guid, recoveryCodeHashes: byte [] list) ->
        setRecoveryCodeHashesQuery (userId, recoveryCodeHashes)