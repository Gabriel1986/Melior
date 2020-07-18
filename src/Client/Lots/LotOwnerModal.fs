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
    PreselectedLotOwner: LotOwner option
    State: State
}
and State =
    | SelectingLotOwnerType
    | LoadingOwners
    | LoadingOrganizations
    | SelectingOwner of list: OwnerListItem list * selected: Guid option
    | SelectingOrganization of list: OrganizationListItem list * selected: Guid option
    | OwnerNotFound
    | OrganizationNotFound
    | Saving
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
    | LotOwnerUpdated of LotOwner
    | LoadOwners
    | OwnersLoaded of OwnerListItem list
    | SelectOwner of OwnerListItem
    | OwnerLoaded of Owner option
    | LoadOrganizations
    | OrganizationsLoaded of OrganizationListItem list
    | SelectOrganization of OrganizationListItem
    | OrganizationLoaded of Organization option
    | Dismiss
    | RemotingError of exn
    | OpenLotOwnerTypeSelection

type LotOwnerModalProps = {|
    IsOpen: bool
    BuildingId: Guid
    LotOwner: LotOwner option
    OnLotOwnerChanged: LotOwner -> unit
    OnCanceled: unit -> unit
|}

let init (props: LotOwnerModalProps) =
    let state, cmd, lotOwnerType =
        match props.LotOwner with
        | Some lotOwner ->
            match lotOwner with
            | LotOwner.Owner _ -> 
                LoadingOwners, Cmd.ofMsg LoadOwners, Some Owner
            | LotOwner.Organization _ ->
                LoadingOrganizations, Cmd.ofMsg LoadOrganizations, Some Organization
        | None -> 
            SelectingLotOwnerType, Cmd.none, None
   
    { 
        IsOpen = props.IsOpen
        BuildingId = props.BuildingId
        LotOwnerType = lotOwnerType
        PreselectedLotOwner = props.LotOwner
        State = state 
    }, cmd

let update onLotOwnerChanged onCanceled message model =
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
    | LotOwnerUpdated s ->
        onLotOwnerChanged s
        model, Cmd.none
    | LoadOwners ->
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).GetOwners {| BuildingId = model.BuildingId |}
                OwnersLoaded
                RemotingError
        { model with State = LoadingOwners }, cmd
    | OwnersLoaded list ->
        let currentlySelectedOwnerId =
            match model.PreselectedLotOwner with
            | Some (LotOwner.Owner o) -> 
                Some (o.Person.PersonId)
            | _ -> 
                None
        { model with State = SelectingOwner (list, currentlySelectedOwnerId) }, Cmd.none
    | SelectOwner owner ->
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).GetOwner owner.PersonId
                OwnerLoaded
                RemotingError
        { model with State = Saving }, cmd
    | OwnerLoaded owner ->
        match owner with
        | Some owner ->
            onLotOwnerChanged (LotOwner.Owner owner)
            model, Cmd.none
        | None ->
            { model with State = OwnerNotFound }, Cmd.none
    | LoadOrganizations ->
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).GetOrganizations {| BuildingId = model.BuildingId |}
                OrganizationsLoaded
                RemotingError
        { model with State = LoadingOrganizations }, cmd
    | OrganizationsLoaded list ->
        let currentlySelectedOrganizationId =
            match model.PreselectedLotOwner with
            | Some (LotOwner.Organization o) -> 
                Some (o.OrganizationId)
            | _ -> 
                None
        { model with State = SelectingOrganization (list, currentlySelectedOrganizationId) }, Cmd.none
    | SelectOrganization organization ->
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).GetOrganization organization.OrganizationId
                OrganizationLoaded
                RemotingError
        { model with State = Saving }, cmd
    | OrganizationLoaded organization ->
        match organization with
        | Some organization ->
            onLotOwnerChanged (LotOwner.Organization organization)
            model, Cmd.none
        | None ->
            { model with State = OrganizationNotFound }, Cmd.none
    | Dismiss ->
        onCanceled()
        model, Cmd.none
    | RemotingError e ->
        { model with State = State.RemotingError e }, Cmd.none

let renderLotOwnerTypeSelection dispatch =
    div [] [
        div [ classes [ Bootstrap.dFlex; Bootstrap.justifyContentCenter; Bootstrap.formInline ] ] [
            label [] [ str "Deze lotOwner is een..." ]
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

let renderOwnerSelectionList (list: OwnerListItem list) (selectedId: Guid option) dispatch =
    SelectionList.render ({
        SelectionMode = SelectionMode.SingleSelect
        LoadItems = fun () -> async {
            return list |> List.sortBy (fun o -> o.FirstName)
        }
        SelectedItems = 
            match selectedId with
            | Some selectedId -> list |> List.filter (fun o -> o.PersonId = selectedId)
            | None -> []
        OnSelectionChanged = fun selection -> selection |> List.head |> SelectOwner |> dispatch
        DisplayListItem = (fun ownerListItem -> 
            [ownerListItem.FirstName; ownerListItem.LastName] 
            |> List.choose id 
            |> String.JoinWith ", "
            |> str)
    }, "OwnerSelectionList")

let renderOrganizationSelectionList (list: OrganizationListItem list) (selectedId: Guid option) dispatch =
    SelectionList.render ({
        SelectionMode = SelectionMode.SingleSelect
        LoadItems = fun () -> async {
            return list |> List.sortBy (fun o -> o.Name)
        }
        SelectedItems = 
            match selectedId with
            | Some selectedId -> list |> List.filter (fun o -> o.OrganizationId = selectedId)
            | None -> []
        OnSelectionChanged = fun selection -> selection |> List.head |> SelectOrganization |> dispatch
        DisplayListItem = (fun org -> str org.Name)
    }, "OrganizationSelectionList")

let modalContent model dispatch =
    match model.State with
    | SelectingLotOwnerType ->
        renderLotOwnerTypeSelection dispatch
    | LoadingOwners ->
        div [] [ str "Eigenaars worden geladen..." ]
    | LoadingOrganizations ->
        div [] [ str "Organisaties worden geladen..." ]
    | SelectingOwner (list, selectedId) ->
        renderOwnerSelectionList list selectedId dispatch
    | SelectingOrganization (list, selectedId) ->
        renderOrganizationSelectionList list selectedId dispatch
    | OwnerNotFound ->
        div [] [ str "De eigenaar werd niet gevonden in de databank, vreemd genoeg..." ]
    | OrganizationNotFound ->
        div [] [ str "De organisatie werd niet gevonden in de databank, vreemd genoeg..." ]        
    | Saving ->
        div [] [ str "Uw wijzigingen worden bewaard" ]
    | State.RemotingError _ ->
        div [] [ str "Er is iets misgelopen bij het ophalen van de gegevens. Gelieve de pagine te verversen en opnieuw te proberen." ]

let renderModalButtons model dispatch =
    let toTypeSelectionButton =
        button 
            [ classes [ Bootstrap.btn; Bootstrap.btnPrimary ]; OnClick (fun _ -> OpenLotOwnerTypeSelection |> dispatch) ] 
            [ str "Selecteer ander type" ] 

    match model.State with
    | SelectingOrganization
    | SelectingOwner _ ->
        [ toTypeSelectionButton ]
    | _ -> 
        []


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
                    yield FooterProp.ShowDismissButton (Some "Annuleren")
                ]
            ]           
        |}

let render (props: LotOwnerModalProps) =
    React.elmishComponent ("LotOwnerModal", init props, update props.OnLotOwnerChanged props.OnCanceled, view)