module Client.Lots.LotDetails

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
open Client.Library

type Model = {
    LotId: Guid
    CurrentBuilding: BuildingListItem
    CurrentUser: User
    State: State
    NotifyCreated: Lot -> unit
    NotifyEdited:  Lot -> unit
}
and State =
    | Loading
    | Viewing  of detail: Lot
    | Editing  of isSaving: bool * lotEditState: LotEditComponent.State
    | Creating of isSaving: bool * lotEditState: LotEditComponent.State
    | LotNotFound

type Msg =
    | LotEditMsg of LotEditComponent.Message
    | View of Lot option
    | Edit of Lot
    | RemotingError of exn
    | Save
    | ProcessCreateResult of Result<Lot, SaveLotError>
    | ProcessUpdateResult of Result<Lot, SaveLotError>

type DetailsProps = {|
    CurrentUser: User
    CurrentBuilding: BuildingListItem
    Identifier: Guid
    IsNew: bool
    NotifyCreated: Lot -> unit
    NotifyEdited: Lot -> unit
|}

let private getLotCmd lotId =
    Cmd.OfAsync.either
        (Remoting.getRemotingApi().GetLot)
        lotId
        View
        RemotingError

let init (props: DetailsProps): Model * Cmd<Msg> =
    let state, cmd =
        if props.IsNew then
            let lot = { Lot.Init props.CurrentBuilding.BuildingId with LotId = props.Identifier }
            let lotEditState, lotEditCmd = LotEditComponent.init lot
            Creating (false, lotEditState), lotEditCmd |> Cmd.map LotEditMsg
        else
            Loading, getLotCmd props.Identifier

    {
        CurrentUser = props.CurrentUser
        CurrentBuilding = props.CurrentBuilding
        LotId = props.Identifier
        State = state
        NotifyCreated = props.NotifyCreated
        NotifyEdited = props.NotifyEdited
    }, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let processSaveLotError =
        function
        | SaveLotError.AuthorizationError ->
            model, showErrorToastCmd "U heeft geen toestemming om deze kavel te bewaren"
        | SaveLotError.NotFound ->
            model, showErrorToastCmd "Het kavel werd niet gevonden in de databank"
        | SaveLotError.Validation errors ->
            match model.State with
            | Creating (_, componentState) ->
                { model with State = Creating (false, { componentState with Errors = errors }) }, Cmd.none
            | Editing (_, componentState) ->
                { model with State = Editing (false, { componentState with Errors = errors }) }, Cmd.none
            | _ ->
                model, Cmd.none

    match msg with
    | View lot ->
        match lot with
        | Some lot ->
            { model with State = Viewing lot }, Cmd.none
        | None ->
            { model with State = LotNotFound }, Cmd.none
    | Edit lot ->
        let lotEditState, lotEditCmd = LotEditComponent.init lot
        { model with State = Editing (false, lotEditState) }, lotEditCmd |> Cmd.map LotEditMsg
    | LotEditMsg componentMsg ->
        let updateComponentState s (isSaving, componentState) =
            let newComponentState, newComponentCmd = LotEditComponent.update componentMsg componentState
            { model with State = s (isSaving, newComponentState) }, 
            newComponentCmd |> Cmd.map LotEditMsg


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
            match ValidatedLot.Validate componentState.Lot with
            | Ok _ ->
                let cmd = 
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().UpdateLot)
                        componentState.Lot
                        (fun result -> result |> Result.map (fun _ -> componentState.Lot) |> ProcessUpdateResult)
                        RemotingError
                { model with State = Editing (true, componentState) }, cmd
            | Error e ->
                { model with State = Editing (false, { componentState with Errors = e }) }, Cmd.none
        | Creating (_, componentState) ->
            match ValidatedLot.Validate componentState.Lot with
            | Ok _ ->
                let cmd =
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().CreateLot)
                        componentState.Lot
                        (fun result -> result |> Result.map (fun _ -> componentState.Lot) |> ProcessCreateResult)
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
            model, Cmd.none
        | Error e ->
            processSaveLotError e
    | ProcessUpdateResult result ->
        match result with
        | Ok result ->
            match model.State with
            | Editing _ -> model.NotifyEdited result
            | _ -> ()
            model, Cmd.none
        | Error e ->
            processSaveLotError e


let view (model: Model) (dispatch: Msg -> unit) =
    match model.State with
    | Loading ->  div [] [ str "Details worden geladen" ]
    | LotNotFound -> div [] [ str "Het door u gekozen kavel werd niet gevonden in de databank..." ]
    | Editing (isSaving, editState)
    | Creating (isSaving, editState) ->
        if isSaving 
        then
            div [] [ str "Het kavel wordt bewaard" ]
        else
            div [] [
                LotEditComponent.view editState (LotEditMsg >> dispatch)

                div [ classes [ Bootstrap.card; Bootstrap.bgLight ] ] [
                    div [ Class Bootstrap.cardBody ] [
                        button [ 
                            classes [ Bootstrap.btn; Bootstrap.btnSuccess ]
                            OnClick (fun _ -> Save |> dispatch) 
                        ] [
                            i [ classes [ FontAwesome.fa; FontAwesome.faSave ] ] []
                            str " "
                            str "Bewaren"
                        ]
                    ]
                ]
            ]
    | Viewing detail ->
        div [] [
            LotViewComponent.render {| Lot = detail |}
            div [ classes [ Bootstrap.card; Bootstrap.bgLight ] ] [
                div [ Class Bootstrap.cardBody ] [
                    button [ 
                        classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
                        OnClick (fun _ -> Edit detail |> dispatch) 
                    ] [
                        i [ classes [ FontAwesome.fa; FontAwesome.faEdit ] ] []
                        str " "
                        str "Aanpassen"
                    ]
                ]
            ]
        ]



let render (props: DetailsProps) =
    React.elmishComponent ("LotDetails", init props, update, view, string props.Identifier)

