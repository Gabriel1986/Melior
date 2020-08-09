module Client.Buildings.BuildingDetails

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Read
open Shared.Remoting
open Shared.Write

open Client
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Library

type Model = {
    BuildingId: Guid
    CurrentUser: User
    State: State
    NotifyCreated: Building -> unit
    NotifyEdited:  Building -> unit
    ShowingSyndicModal: bool
    ShowingConciergeModal: bool
}
and State =
    | Loading
    | Viewing  of detail: Building
    | Editing  of isSaving: bool * buildingEditState: BuildingEditComponent.State
    | Creating of isSaving: bool * buildingEditState: BuildingEditComponent.State
    | BuildingNotFound

type Msg =
    | BuildingEditMsg of BuildingEditComponent.Message
    | View of Building option
    | Edit of Building
    | RemotingError of exn
    | Save
    | ProcessCreateResult of Result<Building, CreateBuildingError>
    | ProcessUpdateResult of Result<Building, UpdateBuildingError>
    | ChangeSyndic
    | SyndicChanged of Syndic option
    | SyndicChangeCanceled
    | ChangeConcierge
    | ConciergeChanged of Concierge option
    | ConciergeChangeCanceled

type DetailsProps = {|
    CurrentUser: User
    Identifier: Guid
    IsNew: bool
    NotifyCreated: Building -> unit
    NotifyEdited: Building -> unit
|}

let private getBuildingCmd buildingId =
    Cmd.OfAsync.either
        (Remoting.getRemotingApi().GetBuilding)
        buildingId
        View
        RemotingError

let init (props: DetailsProps): Model * Cmd<Msg> =
    let state, cmd =
        if props.IsNew then
            let building = { Building.Init () with BuildingId = props.Identifier }
            let buildingEditState, buildingEditCmd = BuildingEditComponent.init building
            Creating (false, buildingEditState), buildingEditCmd |> Cmd.map BuildingEditMsg
        else
            Loading, getBuildingCmd props.Identifier

    {
        CurrentUser = props.CurrentUser
        BuildingId = props.Identifier
        State = state
        NotifyCreated = props.NotifyCreated
        NotifyEdited = props.NotifyEdited
        ShowingConciergeModal = false
        ShowingSyndicModal = false
    }, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let changeViewingState (change: Building -> Building) =
        match model.State with
        | Viewing building -> Viewing (change building)
        | other -> other

    match msg with
    | View building ->
        match building with
        | Some building ->
            { model with State = Viewing building }, Cmd.none
        | None ->
            { model with State = BuildingNotFound }, Cmd.none
    | Edit building ->
        let buildingEditState, buildingEditCmd = BuildingEditComponent.init building
        { model with State = Editing (false, buildingEditState) }, buildingEditCmd |> Cmd.map BuildingEditMsg
    | BuildingEditMsg componentMsg ->
        let updateComponentState s (isSaving, componentState) =
            let newComponentState, newComponentCmd = BuildingEditComponent.update componentMsg componentState
            { model with State = s (isSaving, newComponentState) }, 
            newComponentCmd |> Cmd.map BuildingEditMsg
        match model.State with
        | Editing (isSaving, componentState) ->
            updateComponentState Editing (isSaving, componentState)
        | Creating (isSaving, componentState) ->
            updateComponentState Creating (isSaving, componentState)
        | _ ->
            model, Cmd.none            
    | Save ->
        match model.State with
        | Editing (_, componentState) ->
            match ValidatedBuilding.Validate componentState.Building with
            | Ok _ ->
                let cmd = 
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().UpdateBuilding)
                        componentState.Building
                        (fun result -> result |> Result.map (fun _ -> componentState.Building) |> ProcessUpdateResult)
                        RemotingError
                { model with State = Editing (true, componentState) }, cmd
            | Error e ->
                printf "%A" e
                { model with State = Editing (false, { componentState with Errors = e }) }, Cmd.none
        | Creating (_, componentState) ->
            match ValidatedBuilding.Validate componentState.Building with
            | Ok _ ->
                let cmd =
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().CreateBuilding)
                        componentState.Building
                        (fun result -> result |> Result.map (fun _ -> componentState.Building) |> ProcessCreateResult)
                        RemotingError
                { model with State = Creating(true, componentState) }, cmd
            | Error e ->
                { model with State = Creating(false, { componentState with Errors = e }) }, Cmd.none
        | _ ->
            //Do nothing, unexpected message O_o
            model, Cmd.none
    | RemotingError e ->
        model, showGenericErrorModalCmd e
    | ProcessCreateResult result ->
        match result with
        | Ok result ->
            match model.State with
            | Creating _ -> model.NotifyCreated result
            | _ -> ()
            { model with State = Viewing result }, Cmd.none
        | Error e ->
            match e with
            | CreateBuildingError.AuthorizationError ->
                model, showErrorToastCmd "U hebt geen toestemming om een gebouw aan te maken"
            | CreateBuildingError.Validation errors ->
                match model.State with
                | Creating (_, componentState) ->
                    { model with State = Creating (false, { componentState with Errors = errors }) }, Cmd.none
                | _ ->
                    model, Cmd.none

    | ProcessUpdateResult result ->
        match result with
        | Ok result ->
            match model.State with
            | Editing _ -> model.NotifyEdited result
            | _ -> ()
            { model with State = Viewing result }, Cmd.none
        | Error e ->
            match e with
            | UpdateBuildingError.AuthorizationError ->
                model, showErrorToastCmd "U heeft geen toestemming om dit gebouw te updaten"
            | UpdateBuildingError.NotFound ->
                model, showErrorToastCmd "Het gebouw werd niet gevonden in de databank"
            | UpdateBuildingError.Validation errors ->
                match model.State with
                | Editing (_, componentState) ->
                    { model with State = Creating (false, { componentState with Errors = errors }) }, Cmd.none
                | _ ->
                    model, Cmd.none

    | ChangeSyndic ->
        { model with ShowingSyndicModal = true }, Cmd.none
    | SyndicChangeCanceled ->
        { model with ShowingSyndicModal = false }, Cmd.none
    | SyndicChanged newSyndic ->
        match model.State with
        | Viewing state ->
            let newState = changeViewingState (fun b -> { b with Syndic = newSyndic })
            let cmd = 
                Cmd.OfAsync.attempt
                    (Remoting.getRemotingApi().UpdateBuildingSyndic)
                    (state.BuildingId, newSyndic |> Option.map mapSyndicReadToWrite)
                    (fun e -> RemotingError e)
            { model with ShowingSyndicModal = false; State = newState }, cmd
        | _ ->
            model, Cmd.none
    | ChangeConcierge ->
        { model with ShowingConciergeModal = true }, Cmd.none
    | ConciergeChangeCanceled ->
        { model with ShowingConciergeModal = false }, Cmd.none
    | ConciergeChanged newConcierge ->
        match model.State with
        | Viewing state ->
            let newState = changeViewingState (fun b -> { b with Concierge = newConcierge })
            let cmd =
                Cmd.OfAsync.attempt
                    (Remoting.getRemotingApi().UpdateBuildingConcierge)
                    (state.BuildingId, newConcierge |> Option.map mapConciergeReadToWrite)
                    (fun e -> RemotingError e)
            { model with ShowingConciergeModal = false; State = newState }, cmd
        | _ ->
            model, Cmd.none



let view (model: Model) (dispatch: Msg -> unit) =
    match model.State with
    | Loading ->  div [] [ str "Details worden geladen" ]
    | BuildingNotFound -> div [] [ str "Het door u gekozen gebouw werd niet gevonden in de databank..." ]
    | Editing (isSaving, editState)
    | Creating (isSaving, editState) ->
        if isSaving 
        then
            div [] [ str "Het gebouw wordt bewaard" ]
        else
            div [] [
                BuildingEditComponent.view editState (BuildingEditMsg >> dispatch)

                div [ classes [ Bootstrap.card; Bootstrap.bgLight ] ] [
                    div [ Class Bootstrap.cardBody ] [
                        button [ 
                            classes [ Bootstrap.btn; Bootstrap.btnSuccess ]
                            OnClick (fun _ -> Save |> dispatch) 
                        ] [
                            str "Bewaren"
                        ]
                    ]
                ]
            ]
    | Viewing detail ->
        div [] [
            BuildingViewComponent.render 
                {|
                    Building = detail
                    OnEditSyndic = fun _ -> ChangeSyndic |> dispatch
                    OnDeleteSyndic = fun _ -> SyndicChanged None |> dispatch
                    OnEditConcierge = fun _ -> ChangeConcierge |> dispatch
                    OnDeleteConcierge = fun _ -> ConciergeChanged None |> dispatch
                |}
            div [ classes [ Bootstrap.card; Bootstrap.bgLight ] ] [
                div [ Class Bootstrap.cardBody ] [
                    yield
                        button [ 
                            classes [ Bootstrap.btn; Bootstrap.btnPrimary; Bootstrap.mr1 ]
                            OnClick (fun _ -> Edit detail |> dispatch) 
                        ] [
                            str "Aanpassen"
                        ]

                    if detail.Syndic.IsNone then
                        yield
                            button [
                                classes [ Bootstrap.btn; Bootstrap.btnSecondary; Bootstrap.mr1 ]
                                OnClick (fun _ -> ChangeSyndic |> dispatch)
                            ] [
                                str "Syndicus toekennen"
                            ]

                    if detail.Concierge.IsNone then
                        yield
                            button [
                                classes [ Bootstrap.btn; Bootstrap.btnSecondary ]
                                OnClick (fun _ -> ChangeConcierge |> dispatch)
                            ] [
                                str "Concierge toevoegen"
                            ]
                ]
            ]
            SyndicModal.render 
                {|
                    IsOpen = model.ShowingSyndicModal
                    BuildingId = detail.BuildingId
                    Concierge = detail.Syndic
                    OnConciergeChanged = (fun s -> SyndicChanged (Some s) |> dispatch)
                    OnCanceled = (fun _ -> SyndicChangeCanceled |> dispatch) 
                |}

            ConciergeModal.render
                {|
                    IsOpen = model.ShowingConciergeModal
                    BuildingId = detail.BuildingId
                    Concierge = detail.Concierge
                    OnConciergeChanged = (fun c -> ConciergeChanged (Some c) |> dispatch)
                    OnCanceled = (fun _ -> ConciergeChangeCanceled |> dispatch)
                |}
        ]



let render (props: DetailsProps) =
    React.elmishComponent ("BuildingDetails", init props, update, view, string props.Identifier)

