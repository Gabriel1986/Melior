module Client.Buildings.SyndicModal

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

type Model = {
    IsOpen: bool
    BuildingId: Guid
    SyndicType: SyndicType option
    PreselectedSyndic: Syndic option
    State: State
}
and State =
    | SelectingSyndicType
    | LoadingOwners
    | SelectingOwner of list: OwnerListItem list * selected: Guid option
    | OwnerNotFound
    | LoadingProfessionalSyndics
    | SelectingProfessionalSyndic of list: ProfessionalSyndicListItem list * selected: Guid option
    | ProfessionalSyndicNotFound
    | Saving
    | EditingPerson of PersonEditComponent.State
    | RemotingError of exn
and SyndicType =
    | Owner
    | ProfessionalSyndic
    | Other
    override me.ToString() =
        match me with
        | Owner -> "Eigenaar"
        | ProfessionalSyndic -> "Professionele syndicus"
        | Other -> "Andere"

type Message =
    | SyndicTypeSelected of SyndicType
    | SyndicUpdated of Syndic
    | LoadProfessionalSyndics
    | ProfessionalSyndicsLoaded of ProfessionalSyndicListItem list
    | SelectProfessionalSyndic of ProfessionalSyndicListItem
    | ProfessionalSyndicLoaded of ProfessionalSyndic option
    | LoadOwners
    | OwnersLoaded of OwnerListItem list
    | SelectOwner of OwnerListItem
    | OwnerLoaded of Owner option
    | PersonEditComponentMsg of PersonEditComponent.Message
    | SavePerson
    | Dismiss
    | RemotingError of exn
    | OpenSyndicTypeSelection

type SyndicModalProps = {|
    IsOpen: bool
    BuildingId: Guid
    Concierge: Syndic option
    OnConciergeChanged: Syndic -> unit
    OnCanceled: unit -> unit
|}

let init (props: SyndicModalProps) =
    let state, cmd, syndicType =
        match props.Concierge with
        | Some syndic ->
            match syndic with
            | Syndic.ProfessionalSyndic _ -> 
                LoadingProfessionalSyndics, Cmd.ofMsg LoadProfessionalSyndics, Some ProfessionalSyndic
            | Syndic.Owner _ -> 
                LoadingOwners, Cmd.ofMsg LoadOwners, Some Owner
            | Syndic.Other p ->
                let editingState, editingCmd = PersonEditComponent.init (Some p)
                EditingPerson editingState, editingCmd |> Cmd.map PersonEditComponentMsg, Some Other
        | None -> 
            SelectingSyndicType, Cmd.none, None
   
    { 
        IsOpen = props.IsOpen
        BuildingId = props.BuildingId
        SyndicType = syndicType
        PreselectedSyndic = props.Concierge
        State = state 
    }, cmd

let update onSyndicChanged onCanceled message model =
    match message with
    | OpenSyndicTypeSelection ->
        { model with SyndicType = None; State = SelectingSyndicType }, Cmd.none
    | SyndicTypeSelected syndicType ->
        let newState, newCmd =
            match syndicType with
            | SyndicType.Other ->
                let componentState, componentCmd = PersonEditComponent.init None
                State.EditingPerson componentState, componentCmd |> Cmd.map PersonEditComponentMsg
            | SyndicType.Owner ->
                model.State, Cmd.ofMsg LoadOwners
            | SyndicType.ProfessionalSyndic ->
                model.State, Cmd.ofMsg LoadProfessionalSyndics

        { model with SyndicType = Some syndicType; State = newState }, newCmd
    | SyndicUpdated s ->
        onSyndicChanged s
        model, Cmd.none
    | LoadProfessionalSyndics ->
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).GetProfessionalSyndics ()
                ProfessionalSyndicsLoaded
                RemotingError
        { model with State = LoadingProfessionalSyndics }, cmd
    | ProfessionalSyndicsLoaded list ->
        let selectedPersonId =
            match model.PreselectedSyndic with
            | Some (Syndic.ProfessionalSyndic p) -> 
                Some (p.Organization.OrganizationId)
            | _ -> 
                None
        { model with State = SelectingProfessionalSyndic (list, selectedPersonId) }, Cmd.none
    | SelectProfessionalSyndic proSyndic ->
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).GetProfessionalSyndic proSyndic.OrganizationId
                ProfessionalSyndicLoaded
                RemotingError
        { model with State = Saving }, cmd
    | ProfessionalSyndicLoaded proSyndic ->
        match proSyndic with
        | Some professionalSyndic ->
            onSyndicChanged (Syndic.ProfessionalSyndic professionalSyndic)
            model, Cmd.none
        | None ->
            { model with State = ProfessionalSyndicNotFound }, Cmd.none
    | LoadOwners ->
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).GetOwners {| BuildingId = model.BuildingId |}
                OwnersLoaded
                RemotingError
        { model with State = LoadingOwners }, cmd
    | OwnersLoaded list ->
        let currentlySelectedOwnerId =
            match model.PreselectedSyndic with
            | Some (Syndic.Owner o) -> 
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
    | OwnerLoaded ownerOpt ->
        match ownerOpt with
        | Some owner ->
            onSyndicChanged (Syndic.Owner owner)
            model, Cmd.none
        | None ->
            { model with State = OwnerNotFound }, Cmd.none
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
                onSyndicChanged (Syndic.Other componentState.Person)
                model, Cmd.none
            | Error e ->
                { model with State = EditingPerson { componentState with Errors = e } }, Cmd.none
        | _ ->
            model, Cmd.none
    | Dismiss ->
        onCanceled()
        model, Cmd.none
    | RemotingError e ->
        { model with State = State.RemotingError e }, Cmd.none

let renderSyndicTypeSelection dispatch =
    div [] [
        div [ classes [ Bootstrap.dFlex; Bootstrap.justifyContentCenter; Bootstrap.formInline ] ] [
            label [] [ str "Deze syndicus is een..." ]
            div [ Class Bootstrap.btnGroup ] [
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnPrimary ] 
                    OnClick (fun _ -> SyndicTypeSelected Owner |> dispatch)
                ] [ str (string Owner) ]
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnPrimary ] 
                    OnClick (fun _ -> SyndicTypeSelected ProfessionalSyndic |> dispatch)
                ] [ str (string ProfessionalSyndic) ]
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnPrimary ] 
                    OnClick (fun _ -> SyndicTypeSelected Other |> dispatch)
                ] [ str (string Other) ]
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

let renderProfessionalSyndicList (list: ProfessionalSyndicListItem list) (selectedId: Guid option) dispatch =
    SelectionList.render ({
        SelectionMode = SelectionMode.SingleSelect
        LoadItems = fun () -> async {
            return list |> List.sortBy (fun o -> o.Name)
        }
        SelectedItems = 
            match selectedId with
            | Some selectedId -> list |> List.filter (fun o -> o.OrganizationId = selectedId)
            | None -> []
        OnSelectionChanged = fun selection -> selection |> List.head |> SelectProfessionalSyndic |> dispatch
        DisplayListItem = (fun syndic -> str syndic.Name)
    }, "SyndicSelectionList")

let modalContent model dispatch =
    match model.State with
    | SelectingSyndicType ->
        renderSyndicTypeSelection dispatch
    | LoadingOwners ->
        div [] [ str "Eigenaars worden geladen..." ]
    | SelectingOwner (list, selectedId) ->
        renderOwnerSelectionList list selectedId dispatch
    | LoadingProfessionalSyndics ->
        div [] [ str "Professionele syndici worden gelaten" ]
    | SelectingProfessionalSyndic (list, selectedId) ->
        renderProfessionalSyndicList list selectedId dispatch
    | EditingPerson componentState ->
        PersonEditComponent.view componentState (PersonEditComponentMsg >> dispatch)
    | ProfessionalSyndicNotFound ->
        div [] [ str "De syndicus werd niet gevonden in de databank, vreemd genoeg..." ]
    | OwnerNotFound ->
        div [] [ str "De eigenaar werd niet gevonden in de databank, vreemd genoeg..." ]
    | Saving ->
        div [] [ str "Uw wijzigingen worden bewaard" ]
    | State.RemotingError _ ->
        div [] [ str "Er is iets misgelopen bij het ophalen van de gegevens. Gelieve de pagine te verversen en opnieuw te proberen." ]

let renderModalButtons model dispatch =
    let toTypeSelectionButton =
        button 
            [ classes [ Bootstrap.btn; Bootstrap.btnPrimary ]; OnClick (fun _ -> OpenSyndicTypeSelection |> dispatch) ] 
            [ str "Selecteer ander type" ] 

    match model.State with
    | EditingPerson _ ->
        [ 
            toTypeSelectionButton
            button 
                [ classes [ Bootstrap.btn; Bootstrap.btnSuccess ]; OnClick (fun _ -> SavePerson |> dispatch) ] 
                [ str "Bewaar persoon" ] 
        ]
    | SelectingOwner _
    | SelectingProfessionalSyndic _ ->
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

let render (props: SyndicModalProps) =
    React.elmishComponent ("SyndicModal", init props, update props.OnConciergeChanged props.OnCanceled, view)