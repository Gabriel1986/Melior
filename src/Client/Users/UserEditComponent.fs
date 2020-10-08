module Client.Users.UserEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Client.Library
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Shared.Read

type Message =
    | NameChanged of string
    | EmailAddressChanged of string
    | LanguageChanged of string
    | TwoFacAuthenticationChanged of bool
    | TwoFacAuthenticationChangedConfirmed of bool
    | IsSysAdminChanged of bool
    | IsSysAdminChangedConfirmed of bool
    | UserRoleChanged of BuildingId list
    | SyndicRoleChanged of BuildingId list
    | ProfessionalSyndicRoleChanged of proSyndicIds: Guid list
    | NoOp
    
type State = {
    User: User
    ShowingBuildingSelectionModal: bool
    ShowingProSyndicSelectionModal: bool
    Errors: (string * string) list
}

let init (user: User) =
    {
        User = user
        ShowingBuildingSelectionModal = false
        ShowingProSyndicSelectionModal = false
        Errors = []
    }, Cmd.none

let update (message: Message) (state: State): State * Cmd<Message> =
    let changeUser f =
        { state with User = f state.User }

    let removeError errorName state =
        { state with Errors = (state.Errors |> List.filter (fun (path, e) -> path <> errorName)) }

    match message with
    | NameChanged x ->
        changeUser (fun l -> { l with DisplayName = x })
        |> removeError (nameof state.User.DisplayName), Cmd.none
    | EmailAddressChanged x ->
        changeUser (fun l -> { l with EmailAddress = x })
        |> removeError (nameof state.User.EmailAddress), Cmd.none
    | LanguageChanged x ->
        changeUser (fun l -> { l with PreferredLanguageCode = x })
        |> removeError (nameof state.User.PreferredLanguageCode), Cmd.none
    | NoOp ->
        state, Cmd.none
    | TwoFacAuthenticationChanged x ->
        state, showConfirmationModal ("Deze actie zal de two-factor authentication van de huidige gebruiker afzetten", "Bent u er zeker van?", (fun () -> TwoFacAuthenticationChangedConfirmed x), (fun () -> NoOp))
    | TwoFacAuthenticationChangedConfirmed x ->
        changeUser (fun l -> { l with UseTwoFac = x }), Cmd.none
    | IsSysAdminChanged x ->
        state, 
            if x 
            then showConfirmationModal ("Deze actie zal van de huidige gebruiker een systeem administrator maken", "Bent u er zeker van?", (fun () -> IsSysAdminChangedConfirmed x), (fun () -> NoOp))
            else Cmd.none
    | IsSysAdminChangedConfirmed x ->
        let rolesWithoutSysAdminRole = state.User.Roles |> List.filter (function | SysAdminRole -> false | _ -> true)
        let newRoles = if x then SysAdminRole::rolesWithoutSysAdminRole else rolesWithoutSysAdminRole
        changeUser (fun l -> { l with Roles = newRoles })
        , Cmd.none
    | ProfessionalSyndicRoleChanged syndicIds ->
        let rolesWithoutProSyndicRoles = state.User.Roles |> List.filter (function | ProfessionalSyndicRole _ -> false | _ -> true)
        let newProSyndicRoles = syndicIds |> List.map (fun orgId -> ProfessionalSyndicRole (orgId, []))
        changeUser (fun l -> { l with Roles = newProSyndicRoles @ rolesWithoutProSyndicRoles })
        , Cmd.none
    | SyndicRoleChanged buildingIds ->
        let rolesWithoutSyndicRoles = state.User.Roles |> List.filter (function | SyndicRole _ -> false | _ -> true)
        changeUser (fun l -> { l with Roles = SyndicRole buildingIds::rolesWithoutSyndicRoles })
        , Cmd.none
    | UserRoleChanged buildingIds ->
        let rolesWithoutUserRoles = state.User.Roles |> List.filter (function | UserRole _ -> false | _ -> true)
        changeUser (fun l -> { l with Roles = UserRole buildingIds::rolesWithoutUserRoles })
        , Cmd.none
        

let inColomn columnClass x = div [ Class columnClass ] [ x ]

let view (state: State) (dispatch: Message -> unit) =
    let errorFor (path: string) =
        state.Errors |> List.tryPick (fun (p, error) -> if p = path then Some error else None)

    div [] [
        div [ Class Bootstrap.row ] [
            formGroup [ 
                Label "E-mail"
                Input [ 
                    Type "text"
                    MaxLength 255.0
                    Helpers.valueOrDefault state.User.EmailAddress
                    OnChange (fun e -> EmailAddressChanged e.Value |> dispatch)
                ]
                FormError (errorFor (nameof state.User.EmailAddress))
            ]
            |> inColomn Bootstrap.col4

            formGroup [ 
                Label "Naam"
                Input [ 
                    Type "text"
                    MaxLength 255.0
                    Helpers.valueOrDefault state.User.EmailAddress
                    OnChange (fun e -> EmailAddressChanged e.Value |> dispatch)
                ]
                FormError (errorFor (nameof state.User.EmailAddress))
            ]
            |> inColomn Bootstrap.col4

            //TODO: should probably have a selection box here.
            formGroup [ 
                Label "Taal"
                Input [ 
                    Type "text"
                    MaxLength 16.0
                    Helpers.valueOrDefault state.User.PreferredLanguageCode
                    OnChange (fun e -> LanguageChanged e.Value |> dispatch)
                ]
                FormError (errorFor (nameof state.User.PreferredLanguageCode))
            ]
            |> inColomn Bootstrap.col4
        ]
        div [ Class Bootstrap.row ] [
            formGroup [
                Name "usingTFA"
                Label "TFA afzetten (TFA en codes vergeten...)"
                Input [
                    HTMLAttr.Name "usingTFA"
                    Type "checkbox"
                    Checked state.User.UseTwoFac
                    OnChange (fun _ -> TwoFacAuthenticationChangedConfirmed (not state.User.UseTwoFac) |> dispatch)
                ]
            ]
            |> inColomn Bootstrap.col
        ]
        div [ Class Bootstrap.row ] [
            formGroup [
                Name "isSystemAdmin"
                Label "System admin?"
                Input [
                    HTMLAttr.Name "isSystemAdmin"
                    Type "checkbox"
                    Checked (state.User.IsSysAdmin ())
                    OnChange (fun _ -> TwoFacAuthenticationChangedConfirmed (not (state.User.IsSysAdmin ())) |> dispatch)
                ]
            ]
            |> inColomn Bootstrap.col
        ]
    ]