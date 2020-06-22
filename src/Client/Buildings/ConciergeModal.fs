module Client.Buildings.ConciergeModal

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents
open Shared.Read
open Client.Components
open Client.Components.BasicModal
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Shared.Remoting
open Shared.Library
open Shared.Write

type Model = {
    IsOpen: bool
    BuildingId: Guid
    ConciergeType: ConciergeType option
    PreselectedConcierge: Concierge option
    State: State
}
and State =
    | SelectingConciergeType
    | LoadingOwners
    | SelectingOwner of list: OwnerListItem list * selected: Guid option
    | OwnerNotFound
    | Saving
    | EditingPerson of PersonEditComponent.State
    | SavingPerson of PersonEditComponent.State
    | RemotingError of exn
and ConciergeType =
    | Owner
    | NonOwner
    override me.ToString() =
        match me with
        | Owner -> "Eigenaar"
        | NonOwner -> "Andere"

type Message =
    | ConciergeTypeSelected of ConciergeType
    | ConciergeUpdated of Concierge
    | LoadOwners
    | OwnersLoaded of OwnerListItem list
    | OwnerSelectionChanged of Guid
    | SelectOwner
    | OwnerLoaded of Owner option
    | PersonEditComponentMsg of PersonEditComponent.Message
    | SavePerson
    | PersonSaved
    | CreatePersonError of CreatePersonError
    | UpdatePersonError of UpdatePersonError
    | Dismiss
    | RemotingError of exn
    | OpenConciergeTypeSelection

type ConciergeModalProps = {|
    IsOpen: bool
    BuildingId: Guid
    Concierge: Concierge option
    OnConciergeChanged: Concierge -> unit
    OnCanceled: unit -> unit
|}

let init (props: ConciergeModalProps) =
    let state, cmd, conciergeType =
        match props.Concierge with
        | Some concierge ->
            match concierge with
            | Concierge.Owner _ -> 
                LoadingOwners, Cmd.ofMsg LoadOwners, Some Owner
            | Concierge.NonOwner p ->
                let editingState, editingCmd = PersonEditComponent.init (Some p)
                EditingPerson editingState, editingCmd |> Cmd.map PersonEditComponentMsg, Some NonOwner
        | None -> 
            SelectingConciergeType, Cmd.none, None
   
    { 
        IsOpen = props.IsOpen
        BuildingId = props.BuildingId
        ConciergeType = conciergeType
        PreselectedConcierge = props.Concierge
        State = state 
    }, cmd

let update onConciergeChanged onCanceled message model =
    match message with
    | OpenConciergeTypeSelection ->
        { model with ConciergeType = None; State = SelectingConciergeType }, Cmd.none
    | ConciergeTypeSelected conciergeType ->
        let newState, newCmd =
            match conciergeType with
            | ConciergeType.NonOwner ->
                let componentState, componentCmd = PersonEditComponent.init None
                State.EditingPerson componentState, componentCmd |> Cmd.map PersonEditComponentMsg
            | ConciergeType.Owner ->
                model.State, Cmd.ofMsg LoadOwners

        { model with ConciergeType = Some conciergeType; State = newState }, newCmd
    | ConciergeUpdated s ->
        onConciergeChanged s
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
            match model.PreselectedConcierge with
            | Some (Concierge.Owner o) -> 
                Some (o.Person.PersonId)
            | _ -> 
                None
        { model with State = SelectingOwner (list, currentlySelectedOwnerId) }, Cmd.none
    | OwnerSelectionChanged newSelection ->
        match model.State with
        | SelectingOwner (li, _) ->
            { model with State = SelectingOwner (li, Some newSelection) }, Cmd.none
        | _ ->
            model, Cmd.none
    | SelectOwner ->
        match model.State with
        | SelectingOwner (_, Some selected) ->
            let cmd =
                Cmd.OfAsync.either
                    (Client.Remoting.getRemotingApi()).GetOwner selected
                    OwnerLoaded
                    RemotingError
            { model with State = Saving }, cmd
        | _ ->
            model, Cmd.none
    | OwnerLoaded owner ->
        match owner with
        | Some owner ->
            onConciergeChanged (Concierge.Owner owner)
            model, Cmd.none
        | None ->
            { model with State = OwnerNotFound }, Cmd.none
    | PersonEditComponentMsg x ->
        match model.State with
        | EditingPerson componentState ->
            let newComponentState, newCmd = PersonEditComponent.update x componentState
            { model with State = EditingPerson newComponentState }, newCmd |> Cmd.map PersonEditComponentMsg
        | SavingPerson componentState ->
            let newComponentState, newCmd = PersonEditComponent.update x componentState
            { model with State = SavingPerson newComponentState }, newCmd |> Cmd.map PersonEditComponentMsg
        | _ ->
            model, Cmd.none
    | SavePerson ->
        match model.State with
        | EditingPerson componentState ->
            match ValidatedPerson.Validate componentState.Person with
            | Ok person ->
                let cmd =
                    match componentState.CreateOrUpdate with
                    | Create -> 
                        Cmd.OfAsync.either
                            (Client.Remoting.getRemotingApi()).CreatePerson person
                            (fun result -> match result with | Ok _ -> PersonSaved | Error e -> CreatePersonError e)
                            RemotingError
                    | Update ->
                        Cmd.OfAsync.either
                            (Client.Remoting.getRemotingApi()).UpdatePerson person
                            (fun result -> match result with | Ok _ -> PersonSaved | Error e -> UpdatePersonError e)
                            RemotingError     
                { model with State = SavingPerson componentState }, cmd
            | Error e ->
                { model with State = EditingPerson { componentState with Errors = e } }, Cmd.none
        | _ ->
            model, Cmd.none
    | CreatePersonError e ->
        //TODO
        model, Cmd.none
    | UpdatePersonError e ->
        //TODO
        model, Cmd.none
    | PersonSaved ->
        match model.State with
        | SavingPerson componentState ->
            onConciergeChanged (Concierge.NonOwner componentState.Person)
            model, Cmd.none
        | _ ->
            //Should not occur?
            model, Cmd.none
    | Dismiss ->
        onCanceled()
        model, Cmd.none
    | RemotingError e ->
        { model with State = State.RemotingError e }, Cmd.none

let renderConciergeTypeSelection dispatch =
    div [] [
        div [ classes [ Bootstrap.dFlex; Bootstrap.justifyContentCenter; Bootstrap.formInline ] ] [
            label [] [ str "Deze concierge is een..." ]
            div [ Class Bootstrap.btnGroup ] [
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnPrimary ] 
                    OnClick (fun _ -> ConciergeTypeSelected Owner |> dispatch)
                ] [ str (string Owner) ]
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnPrimary ] 
                    OnClick (fun _ -> ConciergeTypeSelected NonOwner |> dispatch)
                ] [ str (string NonOwner) ]
            ]
        ]
    ]

let renderOwnerSelectionList list selected dispatch =
    div [] [
        str "TODO..."
    ]

let modalContent model dispatch =
    match model.State with
    | SelectingConciergeType ->
        renderConciergeTypeSelection dispatch
    | LoadingOwners ->
        div [] [ str "Eigenaars worden geladen..." ]
    | SelectingOwner (list, selectedId) ->
        renderOwnerSelectionList list selectedId dispatch
    | EditingPerson componentState ->
        PersonEditComponent.view componentState (PersonEditComponentMsg >> dispatch)
    | OwnerNotFound ->
        div [] [ str "De eigenaar werd niet gevonden in de databank, vreemd genoeg..." ]
    | SavingPerson _
    | Saving ->
        div [] [ str "Uw wijzigingen worden bewaard" ]
    | State.RemotingError _ ->
        div [] [ str "Er is iets misgelopen bij het ophalen van de gegevens. Gelieve de pagine te verversen en opnieuw te proberen." ]

let renderModalButtons model dispatch =
    let toTypeSelectionButton =
        button 
            [ classes [ Bootstrap.btn; Bootstrap.btnPrimary ]; OnClick (fun _ -> OpenConciergeTypeSelection |> dispatch) ] 
            [ str "Selecteer ander type" ] 

    match model.State with
    | EditingPerson _ ->
        [ 
            toTypeSelectionButton
            button 
                [ classes [ Bootstrap.btn; Bootstrap.btnSuccess ]; OnClick (fun _ -> SavePerson |> dispatch) ] 
                [ str "Bewaar persoon" ] 
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

let render (props: ConciergeModalProps) =
    React.elmishComponent ("ConciergeModal", init props, update props.OnConciergeChanged props.OnCanceled, view)