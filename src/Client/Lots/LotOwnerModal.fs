module Client.Lots.LotOwnerModal

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Read
open Shared.Remoting
open Shared.Library

open Client.Components
open Client.Components.BasicModal
open Client.Components.SelectionList
open Client.ClientStyle
open Client.ClientStyle.Helpers

type Model = {
    IsOpen: bool
    BuildingId: Guid
    LotOwnerType: LotOwnerType option
    SelectedOwners: OwnerListItem list
    AllOwners: OwnerListItem list
    SelectedOrganizations: OrganizationListItem list
    AllOrganizations: OrganizationListItem list
    State: State
}
and State =
    | SelectingLotOwnerType
    | LoadingOwners
    | LoadingOrganizations
    | SelectingOwners
    | SelectingOrganizations
    | RemotingError of exn
and LotOwnerType =
    | Owner
    | Organization
    override me.ToString() =
        match me with
        | Owner -> "Eigenaar"
        | Organization -> "Organisatie"

type Message =
    | LotOwnerTypeSelected of LotOwnerType
    | LoadOwners
    | OwnersLoaded of OwnerListItem list
    | SetSelectedOwners of OwnerListItem list
    | LoadOrganizations
    | OrganizationsLoaded of OrganizationListItem list
    | SetSelectedOrganizations of OrganizationListItem list
    | Dismiss
    | Save
    | RemotingError of exn
    | OpenLotOwnerTypeSelection

type LotOwnerModalProps = {|
    IsOpen: bool
    BuildingId: Guid
    LotOwners: LotOwner list
    OnOk: LotOwner list -> unit
    OnCanceled: unit -> unit
|}

let init (props: LotOwnerModalProps) =
    let state, cmd, lotOwnerType =
        match props.LotOwners with
        | [] -> 
            SelectingLotOwnerType, Cmd.none, None
        | x when props.LotOwners |> List.forall (fun o -> match o with | LotOwner.Organization -> true | LotOwner.Owner -> false) ->
            LoadingOrganizations, Cmd.ofMsg LoadOrganizations, Some Organization
        | _ ->
            LoadingOwners, Cmd.ofMsg LoadOwners, Some Owner
    { 
        IsOpen = props.IsOpen
        BuildingId = props.BuildingId
        LotOwnerType = lotOwnerType
        AllOwners = []
        AllOrganizations = []
        SelectedOwners = props.LotOwners |> List.choose (function | LotOwner.Owner o -> Some o | _ -> None)
        SelectedOrganizations = props.LotOwners |> List.choose (function | LotOwner.Organization o -> Some o | _ -> None)
        State = state 
    }, cmd

let update onOk onCanceled message model =
    match message with
    | OpenLotOwnerTypeSelection ->
        { model with LotOwnerType = None; State = SelectingLotOwnerType }, Cmd.none
    | LotOwnerTypeSelected lotOwnerType ->
        let newState, newCmd =
            match lotOwnerType with
            | LotOwnerType.Organization ->
                model.State, Cmd.ofMsg LoadOrganizations
            | LotOwnerType.Owner ->
                model.State, Cmd.ofMsg LoadOwners

        { model with LotOwnerType = Some lotOwnerType; State = newState }, newCmd
    | LoadOwners ->
        let cmd =
            match model.AllOwners with
            | [] ->
                Cmd.OfAsync.either
                    (Client.Remoting.getRemotingApi()).GetOwners {| BuildingId = model.BuildingId |}
                    OwnersLoaded
                    RemotingError
            | owners ->
                Cmd.ofMsg (OwnersLoaded owners)
        { model with State = LoadingOwners }, cmd
    | OwnersLoaded list ->
        { model with AllOwners = list; State = SelectingOwners }, Cmd.none
    | SetSelectedOwners owners ->
        { model with SelectedOwners = owners }, Cmd.none
    | LoadOrganizations ->
        let cmd =
            match model.AllOrganizations with
            | [] ->
                Cmd.OfAsync.either
                    (Client.Remoting.getRemotingApi()).GetOrganizations {| BuildingId = model.BuildingId |}
                    OrganizationsLoaded
                    RemotingError
            | orgs ->
                Cmd.ofMsg (OrganizationsLoaded orgs)
        { model with State = LoadingOrganizations }, cmd
    | OrganizationsLoaded list ->
        { model with AllOrganizations = list; State = SelectingOrganizations }, Cmd.none
    | SetSelectedOrganizations organizations ->
        { model with SelectedOrganizations = organizations }, Cmd.none
    | Save ->
        let ownerLotOwners = model.SelectedOwners |> List.map LotOwner.Owner
        let orgLotOwners = model.SelectedOrganizations |> List.map LotOwner.Organization
        onOk (ownerLotOwners @ orgLotOwners)
        model, Cmd.none
    | Dismiss ->
        onCanceled()
        model, Cmd.none
    | RemotingError e ->
        { model with State = State.RemotingError e }, Cmd.none

let renderLotOwnerTypeSelection dispatch =
    div [] [
        div [ classes [ Bootstrap.dFlex; Bootstrap.justifyContentCenter; Bootstrap.formInline ] ] [
            label [] [ str "De eigenaar van deze kavel is een..." ]
            div [ Class Bootstrap.btnGroup ] [
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnPrimary ] 
                    OnClick (fun _ -> LotOwnerTypeSelected Owner |> dispatch)
                ] [ str (string Owner) ]
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnPrimary ] 
                    OnClick (fun _ -> LotOwnerTypeSelected Organization |> dispatch)
                ] [ str (string Organization) ]
            ]
        ]
    ]

let renderOwnerSelectionList (list: OwnerListItem list) (selected: OwnerListItem list) dispatch =
    SelectionList.render (
        {|
            SelectionMode = SelectionMode.MultiSelect
            AllItems = list |> List.sortBy (fun o -> o.FirstName)
            SelectedItems = selected
            OnSelectionChanged = fun selection -> SetSelectedOwners selection |> dispatch
            DisplayListItem = (fun ownerListItem -> 
                [ownerListItem.FirstName; ownerListItem.LastName] 
                |> List.choose id 
                |> String.JoinWith " "
                |> str)
        |}, "OwnerSelectionList")

let renderOrganizationSelectionList (list: OrganizationListItem list) (selected: OrganizationListItem list) dispatch =
    SelectionList.render (
        {|
            SelectionMode = SelectionMode.MultiSelect
            AllItems = list |> List.sortBy (fun o -> o.Name)
            SelectedItems = selected
            OnSelectionChanged = fun selection -> SetSelectedOrganizations selection |> dispatch
            DisplayListItem = (fun org -> str org.Name)
        |}, "OrganizationSelectionList")

let modalContent model dispatch =
    match model.State with
    | SelectingLotOwnerType ->
        renderLotOwnerTypeSelection dispatch
    | LoadingOwners ->
        div [] [ str "Eigenaars worden geladen..." ]
    | LoadingOrganizations ->
        div [] [ str "Organisaties worden geladen..." ]
    | SelectingOwners ->
        renderOwnerSelectionList 
            model.AllOwners 
            model.SelectedOwners
            dispatch
    | SelectingOrganizations ->
        renderOrganizationSelectionList 
            model.AllOrganizations 
            model.SelectedOrganizations
            dispatch
    | State.RemotingError _ ->
        div [] [ str "Er is iets misgelopen bij het ophalen van de gegevens. Gelieve de pagine te verversen en opnieuw te proberen." ]

let renderModalButtons model dispatch =
    let toTypeSelectionButton =
        button 
            [ classes [ Bootstrap.btn; Bootstrap.btnPrimary ]; OnClick (fun _ -> OpenLotOwnerTypeSelection |> dispatch) ] 
            [ str "Selecteer ander type" ]

    let cancelButton =
        button 
            [ classes [ Bootstrap.btn; Bootstrap.btnDanger ]; OnClick (fun _ -> Dismiss |> dispatch) ]
            [ str "Annuleren" ]

    let saveButton =
        button
            [ classes [ Bootstrap.btn; Bootstrap.btnSuccess ]; OnClick (fun _ -> Save |> dispatch) ]
            [ str "Ok" ]

    match model.State with
    | SelectingOrganizations
    | SelectingOwners ->
        [ toTypeSelectionButton; cancelButton; saveButton ]
    | _ -> 
        [ cancelButton ]


let view (model: Model) dispatch =
    BasicModal.render 
        {| 
            ModalProps = [
                IsOpen model.IsOpen
                DisableBackgroundClick true
                OnDismiss (fun _ -> dispatch Dismiss)
                Header [
                    HeaderProp.HasDismissButton true
                ]
                Body [
                    modalContent model dispatch
                ]
                Footer [
                    yield FooterProp.Buttons (renderModalButtons model dispatch)
                ]
            ]           
        |}

let render (props: LotOwnerModalProps) =
    React.elmishComponent ("LotOwnerModal", init props, update props.OnOk props.OnCanceled, view)