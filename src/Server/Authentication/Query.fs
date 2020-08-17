module Server.Authentication.Query
    open System
    open Shared.Read
    open Npgsql.FSharp
    open Server.PostgreSQL
    open Serilog
    open Server.Blueprint.Data.Authentication
    open Google.Authenticator
    open Server.Blueprint.Behavior.ProfessionalSyndics

    type UserDbRow = 
        {
            UserId: Guid
            DisplayName: string
            EmailAddress: string
            PreferredLanguageCode: string
            UseTwoFac: bool
        }
        static member SelectQuery =
            """
                SELECT 
                    UserId, 
                    EmailAddress, 
                    DisplayName, 
                    PreferredLanguageCode,
                    UseTwoFac
                FROM Users
            """

    type RoleDbRow = 
        {
            BuildingId: Guid option
            OrganizationId: Guid option
            Role: string
        }

    let private toRole (cache: IProfessionalSyndicCache) (role: string, dbRow: RoleDbRow list) =
        match role with
        | x when x = nameof Role.UserRole -> 
            match dbRow |> List.choose (fun r -> r.BuildingId) with
            | [] -> []
            | buildingIds -> [ UserRole buildingIds ]
        | x when x = nameof Role.ProfessionalSyndicRole -> 
            match dbRow |> List.choose (fun r -> r.OrganizationId) with
            | [] -> []
            | orgIds ->
                orgIds
                |> List.map (fun orgId -> ProfessionalSyndicRole (orgId, cache.GetBuildingIdsForProfessionalSyndic orgId))
        | x when x = nameof Role.SyndicRole ->
            match dbRow |> List.choose (fun r -> r.BuildingId) with
            | [] -> []
            | buildingIds -> [ SyndicRole buildingIds ]            
        | x when x = nameof Role.SysAdminRole -> 
            [ SysAdminRole ]
        | _ ->
            Log.Logger.Error(sprintf "Role not found: '%s'" role)
            []


    let private findUser (conn: string) (cache: IProfessionalSyndicCache) sqlSearchString parameters = async {
        let! user =
            Sql.connect conn
            |> Sql.query (sprintf "%s %s" UserDbRow.SelectQuery sqlSearchString)
            |> Sql.parameters parameters
            |> Sql.readSingle (fun reader -> { 
                UserId = reader.uuid "UserId"
                DisplayName = reader.string "DisplayName"
                EmailAddress = reader.string "EmailAddress"
                PreferredLanguageCode = reader.string "PreferredLanguageCode"
                UseTwoFac = defaultArg (reader.boolOrNone "UseTwoFac") false
            })

        match user with
        | None -> 
            return None
        | Some userRow ->
            let! roleRows =
                Sql.connect conn
                |> Sql.query "SELECT Role, BuildingId, OrganizationId FROM UserRoles WHERE UserId = @UserId"
                |> Sql.parameters [ "@UserId", Sql.uuid userRow.UserId ]
                |> Sql.read (fun reader -> {
                    Role = reader.string "Role"
                    BuildingId = reader.uuidOrNone "BuildingId"
                    OrganizationId = reader.uuidOrNone "OrganizationId"
                })
            return Some {
                UserId = userRow.UserId
                EmailAddress = userRow.EmailAddress
                DisplayName = userRow.DisplayName
                Roles = roleRows |> List.groupBy (fun r -> r.Role) |> List.collect (toRole cache)
                PreferredLanguageCode = userRow.PreferredLanguageCode
                UseTwoFac = userRow.UseTwoFac
            }
    }

    let getUser (conn: string) (cache: IProfessionalSyndicCache) (userId: Guid) =
        findUser conn cache "WHERE UserId = @UserId" [ "@UserId", Sql.uuid userId ]

    type UserAuth = {
        UserId: Guid
        PasswordHash: byte []
    }

    let authenticateUser (conn: string) (cache: IProfessionalSyndicCache) (pepper: string) (emailAddress: string, password: string) = async {
        let! userAuth =
            Sql.connect conn
            |> Sql.query "SELECT UserId, PasswordHash FROM Users WHERE EmailAddress = @EmailAddress AND IsActive = TRUE"
            |> Sql.parameters [ "@EmailAddress", Sql.string emailAddress ]
            |> Sql.readSingle (fun reader -> {
                UserId = reader.uuid "UserId"
                PasswordHash = reader.bytea "PasswordHash"
            })
        match userAuth with
        | Some userAuth ->            
            if Encryption.verifyPassword pepper (password, userAuth.PasswordHash)
            then return! getUser conn cache userAuth.UserId |> Async.map (fun u -> Ok u.Value)
            else return Error PasswordNotValid
        | None ->
            return Error UserNotFound
    }

    let findUserByEmailAddress (conn: string) (cache: IProfessionalSyndicCache) (emailAddress: string) =
        findUser conn cache "WHERE EmailAddress = @EmailAddress" [ "@EmailAddress", Sql.string emailAddress ]

    let private getTwoFacPassword (conn: string) (encryptionPassword) (userId: Guid) =
        Sql.connect conn
        |> Sql.query "SELECT TwoFacSecret FROM Users WHERE UserId = @UserId"
        |> Sql.parameters [ "@UserId", Sql.uuid userId ]
        |> Sql.readSingle (fun reader -> reader.bytea "TwoFacSecret")
        |> Async.map (function | Some bytes -> Some (Encryption.decryptToString(bytes, encryptionPassword)) | None -> None)

    let getNbFailedTwoFacAttempts (conn: string) (userId: Guid, after: DateTimeOffset) =
        Sql.connect conn
        |> Sql.query "SELECT COUNT(*) AS Count FROM FailedTwoFacEvents WHERE UserId = @UserId AND TimeStamp > @After"
        |> Sql.parameters [ "@UserId", Sql.uuid userId; "@After", Sql.timestamp after.UtcDateTime ]
        |> Sql.readSingle (fun reader -> reader.int "Count")
        |> Async.map (fun count -> count |> Option.defaultValue 0)

    let validateRecoveryCode (conn: string) (pepper: string) (userId: Guid, code: string) =
        Sql.connect conn
        |> Sql.query "SELECT RecoveryCodeHash FROM RecoveryCodes WHERE UserId = @UserId"
        |> Sql.parameters [ "@UserId", Sql.uuid userId ]
        |> Sql.read (fun reader -> reader.bytea "RecoveryCodeHash")
        |> Async.map (List.exists (fun hash -> Encryption.verifyPassword pepper (code, hash)))

    let validateTwoFactorPIN (conn) (twoFacEncryptionPassword) (userId, verificationCode) = async {
        let! twoFacSharedPassword = getTwoFacPassword conn twoFacEncryptionPassword userId
        match twoFacSharedPassword with
        | Some twoFacSharedPassword ->
            let tfa = new TwoFactorAuthenticator();
            return tfa.ValidateTwoFactorPIN(twoFacSharedPassword, verificationCode, TimeSpan.FromMinutes(2.0))
        | None ->
            return false
    }

    let userWithEmailAddressExists (conn) (emailAddress) =
        Sql.connect conn
        |> Sql.query "SELECT COUNT(*) as Count FROM Users WHERE EmailAddress = @EmailAddress"
        |> Sql.parameters [
            "@EmailAddress", Sql.string emailAddress
        ]
        |> Sql.readSingle (fun reader -> reader.int "Count")
        |> Async.map (Option.defaultValue 0 >> (=) 1)