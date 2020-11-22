module Client.Lots.LotOwnerContactModal

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
open Shared.Write

open Client.Components
open Client.Components.BasicModal
open Client.Components.SelectionList
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Library

type CreateOrUpdate =
    | Create
    | Update of LotOwnerContact

type Model = {
    IsOpen: bool
    BuildingId: Guid
    CreateOrUpdate: CreateOrUpdate
    AllOwners: OwnerListItem list
    ContactType: ContactType option
    Contact: LotOwnerContact option
    State: State
    OnContactChanged: LotOwnerContact -> unit
    OnCanceled: unit -> unit
}
and State =
    | SelectingContactType
    | LoadingOwners
    | SelectingOwner
    | OwnerNotFound
    | Saving
    | EditingPerson of PersonEditComponent.State
and ContactType =
    | Owner
    | NonOwner
    override me.ToString() =
        match me with
        | Owner -> "Eigenaar"
        | NonOwner -> "Andere"

type Message =
    | ContactTypeSelected of ContactType
    | LoadOwners
    | OwnersLoaded of OwnerListItem list
    | SelectOwner of OwnerListItem
    | PersonEditComponentMsg of PersonEditComponent.Message
    | SavePerson
    | Dismiss
    | RemotingError of exn
    | OpenContactTypeSelection

type LotOwnerContactModalProps = {|
    IsOpen: bool
    BuildingId: Guid
    CreateOrUpdate: CreateOrUpdate
    OnContactChanged: LotOwnerContact -> unit
    OnCanceled: unit -> unit
|}

let init (props: LotOwnerContactModalProps) =
    let state, cmd, contactType =
        match props.CreateOrUpdate with
        | Update contact ->
            match contact with
            | LotOwnerContact.Owner _ -> 
                LoadingOwners, Cmd.ofMsg LoadOwners, Some Owner
            | LotOwnerContact.NonOwner p ->
                let editingState, editingCmd = PersonEditComponent.init (Some p)
                EditingPerson editingState, editingCmd |> Cmd.map PersonEditComponentMsg, Some NonOwner
        | Create ->
            SelectingContactType, Cmd.none, None
   
    { 
        IsOpen = props.IsOpen
        BuildingId = props.BuildingId
        CreateOrUpdate = props.CreateOrUpdate
        Contact =
            match props.CreateOrUpdate with
            | Create -> None
            | Update contact -> Some contact
        AllOwners = []
        ContactType = contactType
        State = state
        OnContactChanged = props.OnContactChanged
        OnCanceled = props.OnCanceled
    }, cmd

let update (message: Message) (model: Model) =
    match message with
    | OpenContactTypeSelection ->
        { model with ContactType = None; State = SelectingContactType }, Cmd.none
    | ContactTypeSelected conciergeType ->
        let newState, newCmd =
            match conciergeType with
            | ContactType.NonOwner ->
                let componentState, componentCmd = PersonEditComponent.init None
                State.EditingPerson componentState, componentCmd |> Cmd.map PersonEditComponentMsg
            | ContactType.Owner ->
                model.State, 
                    if model.AllOwners.IsEmpty then 
                        Cmd.ofMsg LoadOwners 
                    else 
                        Cmd.ofMsg (OwnersLoaded model.AllOwners)

        { model with ContactType = Some conciergeType; State = newState }, newCmd
    | LoadOwners ->
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).GetOwners model.BuildingId
                OwnersLoaded
                RemotingError
        { model with State = LoadingOwners }, cmd
    | OwnersLoaded list ->
        { model with AllOwners = list; State = SelectingOwner }, Cmd.none
    | SelectOwner owner ->
        //Parent handles this
        model.OnContactChanged (LotOwnerContact.Owner owner)
        model, Cmd.none
    | PersonEditComponentMsg x ->
        match model.State with
        | EditingPerson componentState ->
            let newComponentState, newCmd = PersonEditComponent.update x componentState
            { model with State = EditingPerson newComponentState }, newCmd |> Cmd.map PersonEditComponentMsg        
        | _ ->
            model, Cmd.none
    | SavePerson ->
        match model.State with
        | EditingPerson componentState ->
            match ValidatedPerson.Validate componentState.Person with
            | Ok _ ->
                //Parent handles this
                model.OnContactChanged (LotOwnerContact.NonOwner componentState.Person)
                model, Cmd.none
            | Error e ->
                { model with State = EditingPerson { componentState with Errors = e } }, Cmd.none
        | _ ->
            model, Cmd.none
    | Dismiss ->
        //Parent handles this
        model.OnCanceled()
        model, Cmd.none
    | RemotingError e ->
        model, showGenericErrorModalCmd e

let renderConctactTypeSelection dispatch =
    div [] [
        div [ classes [ Bootstrap.dFlex; Bootstrap.justifyContentCenter; Bootstrap.formInline ] ] [
            label [] [ str "Deze contactpersoon is een..." ]
            div [ classes [ Bootstrap.btnGroup; Bootstrap.ml2 ] ] [
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ] 
                    OnClick (fun _ -> ContactTypeSelected Owner |> dispatch)
                ] [ str (string Owner) ]
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ] 
                    OnClick (fun _ -> ContactTypeSelected NonOwner |> dispatch)
                ] [ str (string NonOwner) ]
            ]
        ]
    ]

let renderOwnerSelectionList (model: Model) (dispatch: Message -> unit) =
    SelectionList.render (
        {|
            SelectionMode = SelectionMode.SingleSelect
            AllItems = model.AllOwners |> List.sortBy (fun o -> o.FirstName)
            SelectedItems =
                match model.Contact with
                | Some (LotOwnerContact.Owner o) -> [ o ]
                | _ -> []
            OnSelectionChanged = fun selected -> SelectOwner (selected |> List.head) |> dispatch
            ListItemToString = (fun ownerListItem ->
                [ownerListItem.FirstName; ownerListItem.LastName] 
                |> List.choose id 
                |> String.joinWith ", ")
        |}, "OwnerSelectionList")

let modalContent model dispatch =
    match model.State with
    | SelectingContactType ->
        renderConctactTypeSelection dispatch
    | LoadingOwners ->
        div [] [ str "Eigenaars worden geladen..." ]
    | SelectingOwner ->
        renderOwnerSelectionList model dispatch
    | EditingPerson componentState ->
        PersonEditComponent.view componentState (PersonEditComponentMsg >> dispatch) {| ShowAddresses = true; ShowBankAccounts = false |}
    | OwnerNotFound ->
        div [] [ str "De eigenaar werd niet gevonden in de databank, vreemd genoeg..." ]
    | Saving ->
        div [] [ str "Uw wijzigingen worden bewaard" ]

let renderModalButtons model dispatch =
    let toTypeSelectionButton =
        button 
            [ classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]; OnClick (fun _ -> OpenContactTypeSelection |> dispatch) ] 
            [
                i [ classes [ FontAwesome.fa; FontAwesome.faArrowLeft ] ] []
                str " "
                str "Selecteer ander type" 
            ] 

    match model.State with
    | EditingPerson _ ->
        [ 
            toTypeSelectionButton
            button 
                [ classes [ Bootstrap.btn; Bootstrap.btnPrimary ]; OnClick (fun _ -> SavePerson |> dispatch) ] 
                [ 
                    i [ classes [ FontAwesome.fa; FontAwesome.faSave ] ] []
                    str " "
                    str "Bewaren" ] 
        ]
    | SelectingOwner _ ->
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

let render (props: LotOwnerContactModalProps) =
    React.elmishComponent ("LotOwnerContactModal", init props, update, view)