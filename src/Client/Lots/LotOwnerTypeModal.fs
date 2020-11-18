module Client.Lots.LotOwnerTypeModal

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
    LotOwnerTypeKind: LotOwnerKind option
    AllOwners: OwnerListItem list
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
and LotOwnerKind =
    | Owner
    | Organization
    override me.ToString() =
        match me with
        | Owner -> "Eigenaar"
        | Organization -> "Leverancier"

type Message =
    | LotOwnerKindSelected of LotOwnerKind
    | LoadOwners
    | OwnersLoaded of OwnerListItem list
    | SetSelectedOwner of OwnerListItem option
    | LoadOrganizations
    | OrganizationsLoaded of OrganizationListItem list
    | SetSelectedOrganization of OrganizationListItem option
    | Dismiss
    | RemotingError of exn
    | OpenLotOwnerTypeSelection

type LotOwnerTypeModalProps = {|
    IsOpen: bool
    BuildingId: Guid
    LotOwnerTypes: LotOwnerType list
    OnSelected: LotOwnerType -> unit
    OnCanceled: unit -> unit
|}

let init (props: LotOwnerTypeModalProps) =
    let state, cmd, lotOwnerType =
        match props.LotOwnerTypes with
        | [] -> 
            SelectingLotOwnerType, Cmd.none, None
        | x when props.LotOwnerTypes |> List.forall (function | LotOwnerType.Organization _ -> true | LotOwnerType.Owner _ -> false) ->
            LoadingOrganizations, Cmd.ofMsg LoadOrganizations, Some Organization
        | _ ->
            LoadingOwners, Cmd.ofMsg LoadOwners, Some Owner
    { 
        IsOpen = props.IsOpen
        BuildingId = props.BuildingId
        LotOwnerTypeKind = lotOwnerType
        AllOwners = []
        AllOrganizations = []
        State = state 
    }, cmd

let update onSelected onCanceled message model =
    match message with
    | OpenLotOwnerTypeSelection ->
        { model with LotOwnerTypeKind = None; State = SelectingLotOwnerType }, Cmd.none
    | LotOwnerKindSelected lotOwnerKind ->
        let newState, newCmd =
            match lotOwnerKind with
            | LotOwnerKind.Organization ->
                model.State, Cmd.ofMsg LoadOrganizations
            | LotOwnerKind.Owner ->
                model.State, Cmd.ofMsg LoadOwners

        { model with LotOwnerTypeKind = Some lotOwnerKind; State = newState }, newCmd
    | LoadOwners ->
        let cmd =
            match model.AllOwners with
            | [] ->
                Cmd.OfAsync.either
                    (Client.Remoting.getRemotingApi()).GetOwners model.BuildingId
                    OwnersLoaded
                    RemotingError
            | owners ->
                Cmd.ofMsg (OwnersLoaded owners)
        { model with State = LoadingOwners }, cmd
    | OwnersLoaded list ->
        { model with AllOwners = list; State = SelectingOwners }, Cmd.none
    | SetSelectedOwner owner ->
        match owner with
        | Some owner -> onSelected(LotOwnerType.Owner owner)
        | None -> onCanceled ()
        model, Cmd.none
    | LoadOrganizations ->
        let cmd =
            match model.AllOrganizations with
            | [] ->
                Cmd.OfAsync.either
                    (Client.Remoting.getRemotingApi()).GetOrganizations model.BuildingId
                    OrganizationsLoaded
                    RemotingError
            | orgs ->
                Cmd.ofMsg (OrganizationsLoaded orgs)
        { model with State = LoadingOrganizations }, cmd
    | OrganizationsLoaded list ->
        { model with AllOrganizations = list; State = SelectingOrganizations }, Cmd.none
    | SetSelectedOrganization organization ->
        match organization with
        | Some organization -> onSelected (LotOwnerType.Organization organization)
        | None -> onCanceled ()
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
            div [ classes [ Bootstrap.btnGroup; Bootstrap.ml2 ] ] [
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ] 
                    OnClick (fun _ -> LotOwnerKindSelected Owner |> dispatch)
                ] [ str (string Owner) ]
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ] 
                    OnClick (fun _ -> LotOwnerKindSelected Organization |> dispatch)
                ] [ str (string Organization) ]
            ]
        ]
    ]

let renderOwnerSelectionList (list: OwnerListItem list) dispatch =
    SelectionList.render (
        {|
            SelectionMode = SelectionMode.SingleSelect
            AllItems = list |> List.sortBy (fun o -> o.FirstName)
            SelectedItems = []
            OnSelectionChanged = fun selection -> SetSelectedOwner (selection |> List.tryHead) |> dispatch
            ListItemToString = (fun ownerListItem -> 
                [ownerListItem.FirstName; ownerListItem.LastName] 
                |> List.choose id 
                |> String.joinWith " ")
        |}, "OwnerSelectionList")

let renderOrganizationSelectionList (list: OrganizationListItem list) dispatch =
    SelectionList.render (
        {|
            SelectionMode = SelectionMode.SingleSelect
            AllItems = list |> List.sortBy (fun o -> o.Name)
            SelectedItems = []
            OnSelectionChanged = fun selection -> SetSelectedOrganization (selection |> List.tryHead) |> dispatch
            ListItemToString = (fun org -> org.Name)
        |}, "OrganizationSelectionList")

let modalContent model dispatch =
    match model.State with
    | SelectingLotOwnerType ->
        renderLotOwnerTypeSelection dispatch
    | LoadingOwners ->
        div [] [ str "Eigenaars worden geladen..." ]
    | LoadingOrganizations ->
        div [] [ str "Leveranciers worden geladen..." ]
    | SelectingOwners ->
        renderOwnerSelectionList 
            model.AllOwners 
            dispatch
    | SelectingOrganizations ->
        renderOrganizationSelectionList 
            model.AllOrganizations 
            dispatch
    | State.RemotingError _ ->
        div [] [ str "Er is iets misgelopen bij het ophalen van de gegevens. Gelieve de pagine te verversen en opnieuw te proberen." ]

let renderModalButtons model dispatch =
    let toTypeSelectionButton =
        button [ 
            classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]
            OnClick (fun _ -> OpenLotOwnerTypeSelection |> dispatch) 
        ] [
            i [ classes [ FontAwesome.fa; FontAwesome.faArrowLeft ] ] []
            str " "
            str "Selecteer ander type"
        ]

    match model.State with
    | SelectingOrganizations
    | SelectingOwners ->
        [ toTypeSelectionButton ]
    | _ -> 
        []


let view (model: Model) dispatch =
    BasicModal.render 
        {| 
            ModalProps = [
                IsOpen model.IsOpen
                DisableBackgroundClick false
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

let render (props: LotOwnerTypeModalProps) =
    React.elmishComponent ("LotOwnerModal", init props, update props.OnSelected props.OnCanceled, view)