module Client.ProfessionalSyndics.ProfessionalSyndicDetails

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
    ProfessionalSyndicId: Guid
    CurrentUser: User
    State: State
    NotifyCreated: ProfessionalSyndic -> unit
    NotifyEdited:  ProfessionalSyndic -> unit
}
and State =
    | Loading
    | Viewing  of detail: ProfessionalSyndic
    | Editing  of isSaving: bool * professionalSyndicEditState: ProfessionalSyndicEditComponent.State
    | Creating of isSaving: bool * professionalSyndicEditState: ProfessionalSyndicEditComponent.State
    | ProfessionalSyndicNotFound

type Msg =
    | ProfessionalSyndicEditComponentMsg of ProfessionalSyndicEditComponent.Message
    | View of ProfessionalSyndic option
    | Edit of ProfessionalSyndic
    | RemotingError of exn
    | Save
    | ProcessCreateResult of Result<ProfessionalSyndic, CreateProfessionalSyndicError>
    | ProcessUpdateResult of Result<ProfessionalSyndic, UpdateProfessionalSyndicError>

type DetailsProps = {|
    CurrentUser: User
    Identifier: Guid
    IsNew: bool
    NotifyCreated: ProfessionalSyndic -> unit
    NotifyEdited: ProfessionalSyndic -> unit
|}

let private getProfessionalSyndicCmd professionalSyndicId =
    Cmd.OfAsync.either
        (Remoting.getRemotingApi().GetProfessionalSyndic)
        professionalSyndicId
        View
        RemotingError

let init (props: DetailsProps): Model * Cmd<Msg> =
    let state, cmd =
        if props.IsNew then
            let tempProfessionalSyndic = ProfessionalSyndic.Init ()
            let professionalSyndic = { tempProfessionalSyndic with Organization = { tempProfessionalSyndic.Organization with OrganizationId = props.Identifier } }
            let professionalSyndicEditState, professionalSyndicEditCmd = ProfessionalSyndicEditComponent.init professionalSyndic
            Creating (false, professionalSyndicEditState), professionalSyndicEditCmd |> Cmd.map ProfessionalSyndicEditComponentMsg
        else
            Loading, getProfessionalSyndicCmd props.Identifier

    {
        CurrentUser = props.CurrentUser
        ProfessionalSyndicId = props.Identifier
        State = state
        NotifyCreated = props.NotifyCreated
        NotifyEdited = props.NotifyEdited
    }, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | View professionalSyndic ->
        match professionalSyndic with
        | Some professionalSyndic ->
            { model with State = Viewing professionalSyndic }, Cmd.none
        | None ->
            { model with State = ProfessionalSyndicNotFound }, Cmd.none
    | Edit professionalSyndic ->
        let professionalSyndicEditState, professionalSyndicEditCmd = ProfessionalSyndicEditComponent.init professionalSyndic
        { model with State = Editing (false, professionalSyndicEditState) }, professionalSyndicEditCmd |> Cmd.map ProfessionalSyndicEditComponentMsg
    | ProfessionalSyndicEditComponentMsg componentMsg ->
        let updateComponentState s (isSaving, componentState) =
            let newComponentState, newComponentCmd = ProfessionalSyndicEditComponent.update componentMsg componentState
            { model with State = s (isSaving, newComponentState) }, 
            newComponentCmd |> Cmd.map ProfessionalSyndicEditComponentMsg

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
            match ValidatedProfessionalSyndic.Validate componentState.ProfessionalSyndic with
            | Ok _ ->
                let cmd = 
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().UpdateProfessionalSyndic)
                        componentState.ProfessionalSyndic
                        (fun result -> result |> Result.map (fun _ -> componentState.ProfessionalSyndic) |> ProcessUpdateResult)
                        RemotingError
                { model with State = Editing (true, componentState) }, cmd
            | Error e ->
                let newComponentState, newComponentCommand = ProfessionalSyndicEditComponent.update (ProfessionalSyndicEditComponent.Message.ErrorsChanged e) (componentState)
                { model with State = Editing (false, newComponentState) }, newComponentCommand |> Cmd.map ProfessionalSyndicEditComponentMsg
        | Creating (_, componentState) ->
            match ValidatedProfessionalSyndic.Validate componentState.ProfessionalSyndic with
            | Ok _ ->
                let cmd =
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().CreateProfessionalSyndic)
                        componentState.ProfessionalSyndic
                        (fun result -> result |> Result.map (fun _ -> componentState.ProfessionalSyndic) |> ProcessCreateResult)
                        RemotingError
                { model with State = Creating(true, componentState) }, cmd
            | Error e ->
                let newComponentState, newComponentCommand = ProfessionalSyndicEditComponent.update (ProfessionalSyndicEditComponent.Message.ErrorsChanged e) (componentState)
                { model with State = Editing (false, newComponentState) }, newComponentCommand |> Cmd.map ProfessionalSyndicEditComponentMsg
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
            | CreateProfessionalSyndicError.AuthorizationError ->
                model, showErrorToastCmd "U heeft geen toestemming om deze professionele syndicus te updaten"
            | CreateProfessionalSyndicError.Validation errors ->
                match model.State with
                | Creating (_, componentState) ->
                    let newComponentState, newComponentCommand = ProfessionalSyndicEditComponent.update (ProfessionalSyndicEditComponent.Message.ErrorsChanged errors) (componentState)
                    { model with State = Creating (false, newComponentState) }, newComponentCommand |> Cmd.map ProfessionalSyndicEditComponentMsg
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
            | UpdateProfessionalSyndicError.AuthorizationError ->
                model, showErrorToastCmd "U heeft geen toestemming om deze professionele syndicus te updaten"
            | UpdateProfessionalSyndicError.NotFound ->
                model, showErrorToastCmd "De professionele syndicus werd niet gevonden in de databank"
            | UpdateProfessionalSyndicError.Validation errors ->
                match model.State with
                | Editing (_, componentState) ->
                    let newComponentState, newComponentCommand = ProfessionalSyndicEditComponent.update (ProfessionalSyndicEditComponent.Message.ErrorsChanged errors) (componentState)
                    { model with State = Editing (false, newComponentState) }, newComponentCommand |> Cmd.map ProfessionalSyndicEditComponentMsg
                | _ ->
                    model, Cmd.none                


let view (model: Model) (dispatch: Msg -> unit) =
    match model.State with
    | Loading ->  div [] [ str "Details worden geladen" ]
    | ProfessionalSyndicNotFound -> div [] [ str "De door u gekozen professionele syndicus werd niet gevonden in de databank..." ]
    | Editing (isSaving, editState)
    | Creating (isSaving, editState) ->
        if isSaving 
        then
            div [] [ str "De professionele syndicus wordt bewaard" ]
        else
            div [] [
                ProfessionalSyndicEditComponent.view editState (ProfessionalSyndicEditComponentMsg >> dispatch)

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
            ProfessionalSyndicViewComponent.render {| ProfessionalSyndic = detail |}
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
    React.elmishComponent ("ProfessionalSyndicDetails", init props, update, view, string props.Identifier)