module Client.Owners.OwnerDetails

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
open Client.Library
open Client.ClientStyle
open Client.ClientStyle.Helpers

type Model = {
    OwnerId: Guid
    CurrentBuilding: BuildingListItem
    CurrentUser: User
    State: State
    NotifyCreated: Owner -> unit
    NotifyEdited:  Owner -> unit
}
and State =
    | Loading
    | Viewing  of detail: Owner
    | Editing  of isSaving: bool * ownerEditState: OwnerEditComponent.State
    | Creating of isSaving: bool * ownerEditState: OwnerEditComponent.State
    | OwnerNotFound

type Msg =
    | OwnerEditComponentMsg of OwnerEditComponent.Message
    | View of Owner option
    | Edit of Owner
    | RemotingError of exn
    | Save
    | ProcessCreateResult of Result<Owner, CreateOwnerError>
    | ProcessUpdateResult of Result<Owner, UpdateOwnerError>

type DetailsProps = {|
    CurrentUser: User
    CurrentBuilding: BuildingListItem
    Identifier: Guid
    IsNew: bool
    NotifyCreated: Owner -> unit
    NotifyEdited: Owner -> unit
|}

let private getOwnerCmd ownerId =
    Cmd.OfAsync.either
        (Remoting.getRemotingApi().GetOwner)
        ownerId
        View
        RemotingError

let init (props: DetailsProps): Model * Cmd<Msg> =
    let state, cmd =
        if props.IsNew then
            let tempOwner = Owner.Init props.CurrentBuilding.BuildingId
            let owner = { tempOwner with Person = { tempOwner.Person with PersonId = props.Identifier } }
            let ownerEditState, ownerEditCmd = OwnerEditComponent.init owner
            Creating (false, ownerEditState), ownerEditCmd |> Cmd.map OwnerEditComponentMsg
        else
            Loading, getOwnerCmd props.Identifier

    {
        CurrentUser = props.CurrentUser
        CurrentBuilding = props.CurrentBuilding
        OwnerId = props.Identifier
        State = state
        NotifyCreated = props.NotifyCreated
        NotifyEdited = props.NotifyEdited
    }, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | View owner ->
        match owner with
        | Some owner ->
            { model with State = Viewing owner }, Cmd.none
        | None ->
            { model with State = OwnerNotFound }, Cmd.none
    | Edit owner ->
        let ownerEditState, ownerEditCmd = OwnerEditComponent.init owner
        { model with State = Editing (false, ownerEditState) }, ownerEditCmd |> Cmd.map OwnerEditComponentMsg
    | OwnerEditComponentMsg componentMsg ->
        let updateComponentState s (isSaving, componentState) =
            let newComponentState, newComponentCmd = OwnerEditComponent.update componentMsg componentState
            { model with State = s (isSaving, newComponentState) }, 
            newComponentCmd |> Cmd.map OwnerEditComponentMsg

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
            match ValidatedOwner.Validate componentState.Owner with
            | Ok _ ->
                let cmd = 
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().UpdateOwner)
                        componentState.Owner
                        (fun result -> result |> Result.map (fun _ -> componentState.Owner) |> ProcessUpdateResult)
                        RemotingError
                { model with State = Editing (true, componentState) }, cmd
            | Error e ->
                let newComponentState, newComponentCommand = OwnerEditComponent.update (OwnerEditComponent.Message.ErrorsChanged e) (componentState)
                { model with State = Editing (false, newComponentState) }, newComponentCommand |> Cmd.map OwnerEditComponentMsg
        | Creating (_, componentState) ->
            match ValidatedOwner.Validate componentState.Owner with
            | Ok _ ->
                let cmd =
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().CreateOwner)
                        componentState.Owner
                        (fun result -> result |> Result.map (fun _ -> componentState.Owner) |> ProcessCreateResult)
                        RemotingError
                { model with State = Creating (true, componentState) }, cmd
            | Error e ->
                let newComponentState, newComponentCommand = OwnerEditComponent.update (OwnerEditComponent.Message.ErrorsChanged e) componentState
                { model with State = Creating (false, newComponentState) }, newComponentCommand |> Cmd.map OwnerEditComponentMsg
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
            | CreateOwnerError.AuthorizationError ->
                model, showErrorToastCmd "U heeft geen toestemming om een eigenaar aan te maken"
            | CreateOwnerError.Validation errors ->
                match model.State with
                | Creating (_, componentState) ->
                    let newComponentState, newComponentCommand = OwnerEditComponent.update (OwnerEditComponent.Message.ErrorsChanged errors) componentState
                    { model with State = Creating (false, newComponentState) }, newComponentCommand |> Cmd.map OwnerEditComponentMsg
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
            | UpdateOwnerError.AuthorizationError ->
                model, showErrorToastCmd "U heeft geen toestemming om deze eigenaar te updaten"
            | UpdateOwnerError.NotFound ->
                model, showErrorToastCmd "De eigenaar werd niet gevonden in de databank"
            | UpdateOwnerError.Validation errors ->
                match model.State with
                | Editing (_, componentState) ->
                    let newComponentState, newComponentCommand = OwnerEditComponent.update (OwnerEditComponent.Message.ErrorsChanged errors) componentState
                    { model with State = Editing (false, newComponentState) }, newComponentCommand |> Cmd.map OwnerEditComponentMsg
                | _ ->
                    model, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    match model.State with
    | Loading ->  div [] [ str "Details worden geladen" ]
    | OwnerNotFound -> div [] [ str "De door u gekozen eigenaar werd niet gevonden in de databank..." ]
    | Editing (isSaving, editState)
    | Creating (isSaving, editState) ->
        if isSaving 
        then
            div [] [ str "De eigenaar wordt bewaard" ]
        else
            div [] [
                OwnerEditComponent.view editState (OwnerEditComponentMsg >> dispatch)

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
            OwnerViewComponent.render {| Building = model.CurrentBuilding; Owner = detail |}
            div [ classes [ Bootstrap.card; Bootstrap.bgLight ] ] [
                div [ Class Bootstrap.cardBody ] [
                    button [ 
                        classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
                        OnClick (fun _ -> Edit detail |> dispatch) 
                    ] [
                        str "Aanpassen"
                    ]
                ]
            ]
        ]



let render (props: DetailsProps) =
    React.elmishComponent ("OwnerDetails", init props, update, view, string props.Identifier)