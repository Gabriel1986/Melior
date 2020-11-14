module Client.Users.UserEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Shared.Read
open Client
open Client.Library
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Components
open Client.Components.BasicModal
open Client.Components.SelectionList
open Shared.Write

module SubComponents =
    let private renderBuildings (onDelete: BuildingListItem -> unit) (buildings: BuildingListItem list) (key: string) =
        SortableTable.render 
            {|
                ListItems = buildings
                DisplayAttributes = Buildings.BuildingsPage.SortableAttribute.All
                IsSelected = None
                OnSelect = None
                IsEditable = None
                OnEdit = None
                IsDeletable = None
                OnDelete = Some onDelete
                Key = key
            |}
    let private renderProfessionalSyndics (onDelete: ProfessionalSyndicListItem -> unit) (proSyndics: ProfessionalSyndicListItem list) =
        SortableTable.render
            {|
                ListItems = proSyndics
                DisplayAttributes = ProfessionalSyndics.ProfessionalSyndicsPage.SortableProfessionalSyndicListItemAttribute.All
                IsSelected = None
                OnSelect = None
                IsEditable = None
                OnEdit = None
                IsDeletable = None
                OnDelete = Some onDelete
                Key = "ProfessionalSyndicRoles"
            |}

    module BuildingBasedRoleViewComponent =
        type BuildingBasedRoleViewComponentProps = {| Title: string; Buildings: Map<Guid, BuildingListItem>; RoleBuildingIds: Guid list; OnBuildingsChanged: BuildingListItem list -> unit |}

        let view (props: BuildingBasedRoleViewComponentProps) =            
            let state = Hooks.useState {| BuildingEditorIsOpenOn = None |}

            let selectedBuildings = props.RoleBuildingIds |> List.choose props.Buildings.TryFind
            div [] [
                fieldset [] [
                    legend [] [
                        h4 [] [ 
                            str props.Title
                            sup [] [
                                a [
                                    classes [ "pointer"; Bootstrap.textPrimary ]
                                    OnClick (fun _ -> state.update {| BuildingEditorIsOpenOn = Some selectedBuildings |}) 
                                    Style [ FontSize "12px"; MarginLeft "2px" ]
                                ][
                                    i [ classes [ FontAwesome.fa; FontAwesome.faEdit ] ] [] 
                                ]
                            ]
                        ]
                    ]
                    match selectedBuildings with
                    | [] ->
                        div [] [ str "Er werden geen gebouwen gevonden" ]
                    | buildings ->
                        let onDeleteBuilding (building: BuildingListItem) =
                            selectedBuildings 
                            |> List.filter (fun b -> b.BuildingId <> building.BuildingId)
                            |> props.OnBuildingsChanged
                        renderBuildings onDeleteBuilding buildings props.Title
                ]

                BasicModal.render 
                    {| 
                        ModalProps = [
                            ModalProp.IsOpen state.current.BuildingEditorIsOpenOn.IsSome
                            ModalProp.OnDismiss (fun () -> state.update {| BuildingEditorIsOpenOn = None |})
                            ModalProp.Header [
                                HeaderProp.HasDismissButton true
                                HeaderProp.Title "Gebouw(en) toevoegen"
                            ]
                            ModalProp.Body [
                                SelectionList.render (
                                    {|
                                        SelectionMode = SelectionMode.MultiSelect
                                        AllItems = 
                                            props.Buildings 
                                            |> Map.toList 
                                            |> List.map snd
                                        SelectedItems = 
                                            match state.current.BuildingEditorIsOpenOn with
                                            | Some buildings -> buildings
                                            | None -> []
                                        OnSelectionChanged = fun selection -> state.update {| BuildingEditorIsOpenOn = Some selection |}
                                        ListItemToString = fun building -> sprintf "%s %s" building.Code building.Name
                                    |}, "BuildingSelectionList")
                            ]
                            ModalProp.Footer [
                                FooterProp.Buttons [
                                    button [ 
                                        classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
                                        OnClick (fun _ ->
                                            state.update {| BuildingEditorIsOpenOn = None |}
                                            props.OnBuildingsChanged (state.current.BuildingEditorIsOpenOn |> Option.defaultValue []))
                                    ] [ str "Ok" ]
                                ]
                                FooterProp.ShowDismissButton (Some "Annuleren")
                            ]
                        ] 
                    |}
            ]

        let render =
            FunctionComponent.Of ((fun (props: BuildingBasedRoleViewComponentProps) -> view props), memoizeWith = memoEqualsButFunctions)

    module ProfessionalSyndicRoleComponent =
        type ProfessionalSyndicRoleComponentProps = {| ProfessionalSyndics: Map<Guid, ProfessionalSyndicListItem>; RoleProfessionalSyndicIds: Guid list; OnProfessionalSyndicsChanged: ProfessionalSyndicListItem list -> unit |}

        let view (props: ProfessionalSyndicRoleComponentProps) =
            let state = Hooks.useState {| ProSyndicEditorIsOpenOn = None |}

            let selectedProfessionalSyndics = props.RoleProfessionalSyndicIds |> List.choose props.ProfessionalSyndics.TryFind
            div [] [
                fieldset [] [
                    legend [] [
                        span [] [
                            h4 [] [ 
                                str "Professionele syndicus rollen"
                                sup [] [
                                    a [
                                        classes [ "pointer"; Bootstrap.textPrimary ]
                                        OnClick (fun _ -> state.update {| ProSyndicEditorIsOpenOn = Some selectedProfessionalSyndics |}) 
                                        Style [ FontSize "12px"; MarginLeft "2px" ]
                                    ][ 
                                        i [ classes [ FontAwesome.fa; FontAwesome.faEdit ] ] [] 
                                    ]
                                ]
                            ]
                        ]
                    ]
                    match selectedProfessionalSyndics with
                    | [] ->
                        div [] [ str "Deze persoon is niet gekoppeld aan een professioneel syndicus kantoor" ]
                    | professionalSyndics ->
                        let onDeleteProSyndic (proSyndic: ProfessionalSyndicListItem) =
                            selectedProfessionalSyndics 
                            |> List.filter (fun b -> b.OrganizationId <> proSyndic.OrganizationId)
                            |> props.OnProfessionalSyndicsChanged
                        renderProfessionalSyndics onDeleteProSyndic professionalSyndics

                    BasicModal.render 
                        {| 
                            ModalProps = [
                                ModalProp.IsOpen state.current.ProSyndicEditorIsOpenOn.IsSome
                                ModalProp.OnDismiss (fun () -> state.update {| ProSyndicEditorIsOpenOn = None |})
                                ModalProp.Header [
                                    HeaderProp.HasDismissButton true
                                    HeaderProp.Title "Syndicus kantoor/kantoren aanpassen"
                                ]
                                ModalProp.Body [
                                    SelectionList.render (
                                        {|
                                            SelectionMode = SelectionMode.MultiSelect
                                            AllItems = 
                                                props.ProfessionalSyndics 
                                                |> Map.toList 
                                                |> List.map snd
                                            SelectedItems = 
                                                match state.current.ProSyndicEditorIsOpenOn with
                                                | Some buildings -> buildings
                                                | None -> []
                                            OnSelectionChanged = fun selection -> state.update {| ProSyndicEditorIsOpenOn = Some selection |}
                                            ListItemToString = fun listItem -> sprintf "%s - %s" listItem.Name (string listItem.Address)
                                        |}, "ProSyndicSelectionList")
                                ]
                                ModalProp.Footer [
                                    FooterProp.Buttons [
                                        button [ 
                                            classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
                                            OnClick (fun _ -> 
                                                state.update {| ProSyndicEditorIsOpenOn = None |}
                                                props.OnProfessionalSyndicsChanged (state.current.ProSyndicEditorIsOpenOn |> Option.defaultValue []))
                                        ] [ str "Ok" ]
                                    ]
                                    FooterProp.ShowDismissButton (Some "Annuleren")
                                ]
                            ] 
                        |}
                ]
            ]

        let render =          
            FunctionComponent.Of ((fun (props: ProfessionalSyndicRoleComponentProps) -> view props), memoizeWith = memoEqualsButFunctions)

type Message =
    | NameChanged of string
    | EmailAddressChanged of string
    | LanguageChanged of string
    | TwoFacAuthenticationChanged of bool
    | TwoFacAuthenticationChangedConfirmed of bool
    | IsSysAdminChanged of bool
    | IsSysAdminChangedConfirmed of bool
    | BuildingsLoaded of BuildingListItem list
    | UserBuildingsChanged of BuildingListItem list
    | SyndicBuildingsChanged of BuildingListItem list
    | ProSyndicsLoaded of ProfessionalSyndicListItem list
    | ProSyndicsChanged of ProfessionalSyndicListItem list
    | NoOp
    | RemotingExceptionOccured of exn
    
type State = {
    User: User
    ShowingBuildingSelectionModal: bool
    ShowingProSyndicSelectionModal: bool
    Errors: (string * string) list
    Buildings: Map<Guid, BuildingListItem>
    ProfessionalSyndics: Map<Guid, ProfessionalSyndicListItem>
}

let init (user: User) =
    let state = {
        User = user
        ShowingBuildingSelectionModal = false
        ShowingProSyndicSelectionModal = false
        Errors = []
        Buildings = Map.empty
        ProfessionalSyndics = Map.empty
    }
    let getBuildingsCmd =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetBuildings)
                None
                BuildingsLoaded
                RemotingExceptionOccured
    let getProSyndicsCmd =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetProfessionalSyndics)
                None
                ProSyndicsLoaded
                RemotingExceptionOccured
    state, Cmd.batch [ getBuildingsCmd; getProSyndicsCmd ]


let update (message: Message) (state: State): State * Cmd<Message> =
    let changeUser f =
        { state with User = f state.User }

    let recalculateValidationErrors (state: State) =
        match state.Errors with
        | [] -> state
        | _errors ->
            match ValidatedUser.Validate state.User with
            | Ok _validated -> state
            | Error validationErrors -> { state with Errors = validationErrors }

    match message with
    | NameChanged x ->
        changeUser (fun l -> { l with DisplayName = x }), Cmd.none
    | EmailAddressChanged x ->
        changeUser (fun l -> { l with EmailAddress = x }), Cmd.none
    | LanguageChanged x ->
        changeUser (fun l -> { l with PreferredLanguageCode = x }), Cmd.none
    | NoOp ->
        state, Cmd.none
    | TwoFacAuthenticationChanged x ->        
        state, 
            if x
            then TwoFacAuthenticationChangedConfirmed x |> Cmd.ofMsg
            else 
                showConfirmationModal 
                    {|
                        Title = "Deze actie zal de two-factor authentication van de huidige gebruiker afzetten"
                        Message = "Bent u er zeker van?"
                        OnConfirmed = fun () -> TwoFacAuthenticationChangedConfirmed x
                        OnDismissed = fun () -> NoOp
                    |}
    | TwoFacAuthenticationChangedConfirmed x ->
        changeUser (fun l -> { l with UseTwoFac = x }), Cmd.none
    | IsSysAdminChanged x ->
        state, 
            if x 
            then 
                showConfirmationModal 
                    {|
                        Title = "Deze actie zal van de huidige gebruiker een systeem administrator maken"
                        Message = "Bent u er zeker van?"
                        OnConfirmed = fun () -> IsSysAdminChangedConfirmed x
                        OnDismissed = fun () -> NoOp
                    |}
            else IsSysAdminChangedConfirmed x |> Cmd.ofMsg
    | IsSysAdminChangedConfirmed x ->
        let rolesWithoutSysAdminRole = state.User.Roles |> List.filter (function | SysAdminRole -> false | _ -> true)
        let newRoles = if x then SysAdminRole::rolesWithoutSysAdminRole else rolesWithoutSysAdminRole
        changeUser (fun l -> { l with Roles = newRoles })
        , Cmd.none
    | ProSyndicsLoaded professionalSyndics ->
        { state with ProfessionalSyndics = professionalSyndics |> List.map (fun s -> s.OrganizationId, s) |> Map.ofList }
        , Cmd.none
    | ProSyndicsChanged professionalSyndics ->
        let rolesWithoutProSyndicRoles =
            state.User.Roles
            |> List.filter (function | ProfessionalSyndicRole _ -> false | _ -> true)
        let newProSyndicRoles = 
            professionalSyndics 
            |> List.sortBy (fun org -> org.Name)
            |> List.map (fun professionalSyndic -> ProfessionalSyndicRole (professionalSyndic.OrganizationId, []))
        changeUser (fun l -> { l with Roles = rolesWithoutProSyndicRoles @ newProSyndicRoles }), Cmd.none
    | BuildingsLoaded buildings ->
        { state with Buildings = buildings |> List.map (fun b -> b.BuildingId, b) |> Map.ofList }, Cmd.none
    | UserBuildingsChanged buildings ->
        let rolesWithoutUserRoles =
            state.User.Roles
            |> List.filter (function | UserRole _ -> false | _ -> true)
        let newUserRole =
            UserRole (buildings |> List.sortBy (fun b -> b.Code) |> List.map (fun b -> b.BuildingId))
        changeUser (fun l -> { l with Roles = newUserRole::rolesWithoutUserRoles }), Cmd.none
    | SyndicBuildingsChanged buildings ->
        let rolesWithoutSyndicRoles =
            state.User.Roles
            |> List.filter (function | SyndicRole _ -> false | _ -> true)
        let newSyndicRole =
            SyndicRole (buildings |> List.sortBy (fun b -> b.Code) |> List.map (fun b -> b.BuildingId))
        changeUser (fun l -> { l with Roles = newSyndicRole::rolesWithoutSyndicRoles }), Cmd.none
    | RemotingExceptionOccured error ->
        state, showGenericErrorModalCmd error

    |> (fun (state, cmd) -> state |> recalculateValidationErrors, cmd)

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
                    Helpers.valueOrDefault state.User.DisplayName
                    OnChange (fun e -> NameChanged e.Value |> dispatch)
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
            div [] [
                if state.User.UseTwoFac then yield button [ OnClick (fun _ -> TwoFacAuthenticationChanged false |> dispatch) ] [ str "Two-factor authenticatie afzetten (GSM verloren, applicatie er af gesmeten, codes verloren, ...)" ]
                if state.User.IsSysAdmin ()
                then yield button [ classes [ Bootstrap.btn; Bootstrap.btnOutlineDanger ]; OnClick (fun _ -> IsSysAdminChanged false |> dispatch) ] [ str "Systeembeheerder rol afnemen" ]
                else yield button [ classes [ Bootstrap.btn; Bootstrap.btnOutlineDanger ]; OnClick (fun _ -> IsSysAdminChanged true |> dispatch) ] [ str "Systeembeheerder rol toekennen" ]
            ]
            |> inColomn Bootstrap.col
        ]

        fieldset [ Class Bootstrap.mt2 ] [
            legend [] [ h3 [] [ str "Rollen" ] ]
            div [ Class Bootstrap.mt2 ] [
                SubComponents.BuildingBasedRoleViewComponent.render 
                    {| 
                        Title = "Gebruikersrollen"
                        Buildings = state.Buildings
                        RoleBuildingIds = state.User.Roles |> List.collect (function | UserRole buildingIds -> buildingIds | _ -> [])
                        OnBuildingsChanged = fun buildings -> UserBuildingsChanged buildings |> dispatch
                    |}
            ]
            div [ Class Bootstrap.mt2 ] [
                SubComponents.BuildingBasedRoleViewComponent.render
                    {|
                        Title = "Eigenaar syndicus rollen"
                        Buildings = state.Buildings
                        RoleBuildingIds = state.User.Roles |> List.collect (function | SyndicRole buildingIds -> buildingIds | _ -> [])
                        OnBuildingsChanged = fun buildings -> SyndicBuildingsChanged buildings |> dispatch
                    |}
            ]
            div [ Class Bootstrap.mt2 ] [
                SubComponents.ProfessionalSyndicRoleComponent.render
                    {|
                        ProfessionalSyndics = state.ProfessionalSyndics
                        RoleProfessionalSyndicIds = state.User.Roles |> List.choose (function | ProfessionalSyndicRole (orgId, _) -> Some orgId | _ -> None)
                        OnProfessionalSyndicsChanged = fun proSyndics -> ProSyndicsChanged proSyndics |> dispatch
                    |}
            ]
        ]
    ]