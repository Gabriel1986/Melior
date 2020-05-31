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

type Model = {
    BuildingId: Guid
    CurrentUser: CurrentUser
    State: State
    NotifyCreated: Building -> unit
    NotifyEdited:  Building -> unit
}
and State =
    | Loading
    | Viewing  of detail: Building
    | Editing  of isSaving: bool * buildingEditState: BuildingEditComponent.State
    | Creating of isSaving: bool * buildingEditState: BuildingEditComponent.State
    | BuildingNotFound
    | RemotingError of exn

type Msg =
    | BuildingEditMsg of BuildingEditComponent.Message
    | View of Building option
    | Edit of Building
    | RemotingError of exn
    | Save
    | ProcessCreateResult of Result<Building, CreateBuildingError>
    | ProcessUpdateResult of Result<Building, UpdateBuildingError>

type DetailsProps = {|
    CurrentUser: CurrentUser
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
    }, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
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
            | Ok building ->
                let cmd = 
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().UpdateBuilding)
                        building
                        (fun result -> result |> Result.map (fun _ -> componentState.Building) |> ProcessUpdateResult)
                        RemotingError
                { model with State = Editing (true, componentState) }, cmd
            | Error e ->
                printf "%A" e
                { model with State = Editing (false, { componentState with Errors = e }) }, Cmd.none
        | Creating (_, componentState) ->
            match ValidatedBuilding.Validate componentState.Building with
            | Ok building ->
                let cmd =
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().CreateBuilding)
                        building
                        (fun result -> result |> Result.map (fun _ -> componentState.Building) |> ProcessCreateResult)
                        RemotingError
                { model with State = Creating(true, componentState) }, cmd
            | Error e ->
                { model with State = Creating(false, { componentState with Errors = e }) }, Cmd.none
        | _ ->
            //Do nothing, unexpected message O_o
            model, Cmd.none
    | RemotingError e ->
        { model with State = State.RemotingError e }, Cmd.none
    | ProcessCreateResult result ->
        match result with
        | Ok result ->
            match model.State with
            | Creating _ -> model.NotifyCreated result
            | _ -> ()
            { model with State = Viewing result }, Cmd.none
        | Error e ->
            //TODO!
            model, Cmd.none

    | ProcessUpdateResult result ->
        match result with
        | Ok result ->
            match model.State with
            | Editing _ -> model.NotifyEdited result
            | _ -> ()
            { model with State = Viewing result }, Cmd.none
        | Error e ->
            //TODO!
            model, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    match model.State with
    | Loading ->  div [] [ str "Details worden geladen" ]
    | BuildingNotFound -> div [] [ str "Het door u gekozen gebouw werd niet gevonden in de databank..." ]
    | State.RemotingError _ -> div [] [ str "Er is iets misgelopen bij het ophalen van de gegevens, gelieve de pagina te verversen" ]
    | Editing (isSaving, editState)
    | Creating (isSaving, editState) ->
        if isSaving 
        then
            div [] [ str "Het gebouw wordt bewaard" ]
        else
            div [] [
                BuildingEditComponent.view editState (BuildingEditMsg >> dispatch)
                div [] [
                    button [ 
                        classes [ Bootstrap.btn; Bootstrap.btnSuccess ]
                        OnClick (fun _ -> Save |> dispatch) 
                    ] [
                        str "Bewaren"
                    ]
                ]
            ]
    | Viewing detail ->
        div [] [
            BuildingViewComponent.render {| Building = detail |}
            div [] [
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
                    OnClick (fun _ -> Edit detail |> dispatch) 
                ] [
                    str "Aanpassen"
                ]
            ]
        ]



let render (props: DetailsProps) =
    React.elmishComponent ("BuildingDetails", init props, update, view, string props.Identifier)

