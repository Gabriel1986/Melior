module Client.Buildings.SyndicModal

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents
open Shared.Domain
open Client.Components
open Client.Components.BasicModal
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Shared.Remoting
open Shared.Library

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
    | SavingPerson of PersonEditComponent.State
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
    | ProfessionalSyndicSelectionChanged of Guid
    | SelectProfessionalSyndic
    | ProfessionalSyndicLoaded of ProfessionalSyndic option
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
        let currentlySelectedSyndicId =
            match model.PreselectedSyndic with
            | Some (Syndic.ProfessionalSyndic p) -> 
                Some (p.ProfessionalSyndicId)
            | _ -> 
                None
        { model with State = SelectingProfessionalSyndic (list, currentlySelectedSyndicId) }, Cmd.none
    | ProfessionalSyndicSelectionChanged newSelection ->
        match model.State with
        | SelectingProfessionalSyndic (li, _) ->
            { model with State = SelectingProfessionalSyndic (li, Some newSelection) }, Cmd.none
        | _ ->
            model, Cmd.none
    | SelectProfessionalSyndic ->
        match model.State with
        | SelectingProfessionalSyndic (_, Some selected) ->
            let cmd =
                Cmd.OfAsync.either
                    (Client.Remoting.getRemotingApi()).GetProfessionalSyndic selected
                    ProfessionalSyndicLoaded
                    RemotingError
            { model with State = Saving }, cmd
        | _ ->
            model, Cmd.none
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
            onSyndicChanged (Syndic.Owner owner)
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
            let cmd =
                match componentState.CreateOrUpdate with
                | Create -> 
                    Cmd.OfAsync.either
                        (Client.Remoting.getRemotingApi()).CreatePerson componentState.Person
                        (fun result -> match result with | Ok _ -> PersonSaved | Error e -> CreatePersonError e)
                        RemotingError
                | Update ->
                    Cmd.OfAsync.either
                        (Client.Remoting.getRemotingApi()).UpdatePerson componentState.Person
                        (fun result -> match result with | Ok _ -> PersonSaved | Error e -> UpdatePersonError e)
                        RemotingError     
            { model with State = SavingPerson componentState }, cmd
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
            onSyndicChanged (Syndic.Other componentState.Person)
            model, Cmd.none
        | _ ->
            //Should not occur?
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

let renderOwnerSelectionList list selected dispatch =
    div [] [
        str "TODO..."
    ]

let renderProfessionalSyndicList list selected dispatch =
    div [] [
        str "TODO..."
    ]

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
    | SavingPerson _
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
                    HeaderProp.ShowDismissButton true
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