module Client.Financial.DistributionKeys.DistributionKeyDetails

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Read
open Shared.Remoting

open Client
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Library
open Types

type Model = {
    AllLots: LotListItem list
    DistributionKeyId: Guid
    State: State
    NotifyCreated: DistributionKeyModel -> unit
    NotifyEdited: DistributionKeyModel -> unit
}
and State =
    | Loading
    | Viewing  of detail: DistributionKeyModel
    | Editing  of isSaving: bool * distributionKeyEditState: DistributionKeyEditComponent.State
    | Creating of isSaving: bool * distributionKeyEditState: DistributionKeyEditComponent.State
    | DistributionKeyNotFound

type Msg =
    | EditComponentMsg of DistributionKeyEditComponent.Message
    | View of DistributionKeyModel option
    | Edit of DistributionKeyModel
    | RemotingError of exn
    | Save
    | ProcessCreateResult of Result<DistributionKeyModel, SaveDistributionKeyError>
    | ProcessUpdateResult of Result<DistributionKeyModel, SaveDistributionKeyError>

type DetailsProps = {|
    CurrentUser: User
    CurrentBuildingId: Guid option
    Identifier: Guid
    IsNew: bool
    AllLots: LotListItem list
    NotifyCreated: DistributionKeyModel -> unit
    NotifyEdited: DistributionKeyModel -> unit
|}

let private getDistributionKey lots distributionKeyId =
    Cmd.OfAsync.either
        (Client.Remoting.getRemotingApi()).GetDistributionKey
        distributionKeyId
        (Option.map (DistributionKeyModel.FromBackendModel lots) >> View)
        RemotingError

let init (props: DetailsProps): Model * Cmd<Msg> =
    let model = { 
        AllLots = props.AllLots
        DistributionKeyId = props.Identifier
        State = Loading
        NotifyCreated = props.NotifyCreated
        NotifyEdited = props.NotifyEdited
    }
    if props.IsNew then
        let distributionKey = { DistributionKeyModel.Init (props.CurrentBuildingId) with DistributionKeyId = props.Identifier }
        let editState, editCmd = DistributionKeyEditComponent.init (distributionKey, props.AllLots)
        { model with State = Creating (false, editState) }, editCmd |> Cmd.map EditComponentMsg
    else
        { model with State = Loading }, getDistributionKey props.AllLots props.Identifier

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let processSaveError =
        function
        | SaveDistributionKeyError.AuthorizationError ->
            model, showErrorToastCmd "U heeft geen toestemming om deze verdeelsleutel te bewaren"
        | SaveDistributionKeyError.NotFound ->
            model, showErrorToastCmd "De verdeelsleutel werd niet gevonden in de databank"
        | SaveDistributionKeyError.Validation errors ->
            match model.State with
            | Creating (_, componentState) ->
                { model with State = Creating (false, { componentState with Errors = errors }) }, Cmd.none                
            | Editing (_, componentState) ->
                { model with State = Editing (false, { componentState with Errors = errors }) }, Cmd.none
            | _ ->
                model, Cmd.none

    match msg with
    | View distributionKey ->
        match distributionKey with
        | Some distributionKey ->
            { model with State = Viewing distributionKey }, Cmd.none
        | None ->
            { model with State = DistributionKeyNotFound }, Cmd.none
    | Edit distributionKey ->
        let editState, editCmd = DistributionKeyEditComponent.init (distributionKey, model.AllLots)
        { model with State = Editing (false, editState) }, editCmd |> Cmd.map EditComponentMsg
    | EditComponentMsg componentMsg ->
        let updateComponentState s (isSaving, componentState) =
            let newComponentState, newComponentCmd = DistributionKeyEditComponent.update componentMsg componentState
            { model with State = s (isSaving, newComponentState) }, 
            newComponentCmd |> Cmd.map EditComponentMsg
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
            match componentState.DistributionKey.Validate () with
            | Ok _ when componentState.DistributionKey.CanBeEdited ->
                let cmd = 
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().UpdateDistributionKey)
                        (componentState.DistributionKey.ToBackendModel())
                        (fun result -> result |> Result.map (fun _ -> componentState.DistributionKey) |> ProcessUpdateResult)
                        RemotingError
                { model with State = Editing (true, componentState) }, cmd
            | Ok _ ->
                printf "This should not be possible..."
                model, showErrorToastCmd "Deze verdeelsleutel kan niet gewijzigd worden"
            | Error e ->
                printf "%A" e
                { model with State = Editing (false, { componentState with Errors = e }) }, Cmd.none
        | Creating (_, componentState) ->
            match componentState.DistributionKey.Validate () with
            | Ok _ ->
                let cmd =
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().CreateDistributionKey)
                        (componentState.DistributionKey.ToBackendModel())
                        (fun result -> result |> Result.map (fun _ -> componentState.DistributionKey) |> ProcessCreateResult)
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
            processSaveError e

    | ProcessUpdateResult result ->
        match result with
        | Ok result ->
            match model.State with
            | Editing _ -> model.NotifyEdited result
            | _ -> ()
            { model with State = Viewing result }, Cmd.none
        | Error e ->
            processSaveError e

let view (model: Model) (dispatch: Msg -> unit) =
    match model.State with
    | Loading ->  div [] [ str "Details worden geladen" ]
    | DistributionKeyNotFound -> div [] [ str "De door u gekozen verdeelsleutel werd niet gevonden in de databank..." ]
    | Editing (isSaving, editState)
    | Creating (isSaving, editState) ->
        if isSaving 
        then
            div [] [ str "De verdeelsleutel wordt bewaard" ]
        else
            div [] [
                DistributionKeyEditComponent.view editState (EditComponentMsg >> dispatch)

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
            DistributionKeyViewComponent.render 
                {|
                    DistributionKey = detail
                    AllLots = model.AllLots
                |}
            if detail.CanBeEdited then
                div [ classes [ Bootstrap.card; Bootstrap.bgLight ] ] [
                    div [ Class Bootstrap.cardBody ] [
                        button [ 
                            classes [ Bootstrap.btn; Bootstrap.btnPrimary; Bootstrap.mr1 ]
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
    React.elmishComponent ("DistributionKeyDetails", init props, update, view)