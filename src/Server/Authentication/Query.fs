module Server.Authentication.Query
    open System
    open Shared.Read
    open Npgsql.FSharp
    open Server.PostgreSQL
    open Serilog
    open Server.Blueprint.Data.Authentication

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
            Role: string
        }
        member me.ToRole () =
            match me.Role with
            | x when x = nameof (Role.User) -> me.BuildingId |> Option.map Role.User
            | x when x = nameof (Role.ProfessionalSyndic) -> me.BuildingId |> Option.map Role.ProfessionalSyndic
            | x when x = nameof (Role.Syndic) -> me.BuildingId |> Option.map Role.Syndic
            | x when x = nameof (Role.SysAdmin) -> Some Role.SysAdmin
            | _ -> 
                Log.Logger.Error(sprintf "Role not found: '%s'" me.Role)
                None

    let private toRole (role: RoleDbRow) = role.ToRole()

    let private findUser (conn: string) sqlSearchString parameters = async {
        let! user =
            Sql.connect conn
            |> Sql.query (sprintf "%s %s" UserDbRow.SelectQuery sqlSearchString)
            |> Sql.parameters parameters
            |> Sql.readSingle (fun reader -> { 
                UserId = reader.uuid "UserId"
                DisplayName = reader.string "DisplayName"
                EmailAddress = reader.string "EmailAddress"
                PreferredLanguageCode = reader.string "PreferredLanguageCode"
                UseTwoFac = reader.bool "UseTwoFac"
            })

        match user with
        | None -> 
            return None
        | Some userRow ->
            let! roleRows =
                Sql.connect conn
                |> Sql.query "SELECT Role, BuildingId FROM UserRoles WHERE UserId = @UserId"
                |> Sql.parameters [ "@UserId", Sql.uuid userRow.UserId ]
                |> Sql.read (fun reader -> {
                    Role = reader.string "Role"
                    BuildingId = reader.uuidOrNone "BuildingId"
                })
            return Some {
                UserId = userRow.UserId
                EmailAddress = userRow.EmailAddress
                DisplayName = userRow.DisplayName
                Roles = roleRows |> List.choose toRole
                PreferredLanguageCode = userRow.PreferredLanguageCode
                UseTwoFac = userRow.UseTwoFac
            }
    }

    let getUser (conn: string) (userId: Guid) =
        findUser conn "WHERE UserId = @UserId" [ "@UserId", Sql.uuid userId ]

    type UserAuth = {
        UserId: Guid
        PasswordHash: byte []
    }

    let authenticateUser (conn: string) (pepper: string) (emailAddress: string, password: string) = async {
        let! userAuth =
            Sql.connect conn
            |> Sql.query "SELECT UserId, PasswordHash FROM Users WHERE EmailAddress = @EmailAddress AND IsActive = 1"
            |> Sql.parameters [ "@EmailAddress", Sql.string emailAddress ]
            |> Sql.readSingle (fun reader -> {
                UserId = reader.uuid "UserId"
                PasswordHash = reader.bytea "PasswordHash"
            })
        match userAuth with
        | Some userAuth ->            
            if Encryption.verifyPassword pepper (password, userAuth.PasswordHash)
            then return! getUser conn userAuth.UserId |> Async.map (fun u -> Ok u.Value)
            else return Error PasswordNotValid
        | None ->
            return Error UserNotFound
    }

    let findUserByEmailAddress (conn: string) (emailAddress: string) =
        findUser conn "WHERE EmailAddress = @EmailAddress" [ "@EmailAddress", Sql.string emailAddress ]

    let getTwoFacPassword (conn: string) (encryptionPassword) (userId: Guid) =
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