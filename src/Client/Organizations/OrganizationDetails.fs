module Client.Organizations.OrganizationDetails

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
    OrganizationId: Guid
    CurrentBuilding: BuildingListItem
    CurrentUser: CurrentUser
    State: State
    NotifyCreated: Organization -> unit
    NotifyEdited:  Organization -> unit
}
and State =
    | Loading
    | Viewing  of detail: Organization
    | Editing  of isSaving: bool * organizationEditState: OrganizationEditComponent.State
    | Creating of isSaving: bool * organizationEditState: OrganizationEditComponent.State
    | OrganizationNotFound
    | RemotingError of exn

type Msg =
    | OrganizationEditMsg of OrganizationEditComponent.Message
    | View of Organization option
    | Edit of Organization
    | RemotingError of exn
    | Save
    | ProcessCreateResult of Result<Organization, CreateOrganizationError>
    | ProcessUpdateResult of Result<Organization, UpdateOrganizationError>

type DetailsProps = {|
    CurrentUser: CurrentUser
    CurrentBuilding: BuildingListItem
    Identifier: Guid
    IsNew: bool
    NotifyCreated: Organization -> unit
    NotifyEdited: Organization -> unit
|}

let private getOrganizationCmd organizationId =
    Cmd.OfAsync.either
        (Remoting.getRemotingApi().GetOrganization)
        organizationId
        View
        RemotingError

let init (props: DetailsProps): Model * Cmd<Msg> =
    let state, cmd =
        if props.IsNew then
            let organization = { Organization.Init (Some props.CurrentBuilding.BuildingId) with OrganizationId = props.Identifier }
            let organizationEditState, organizationEditCmd = OrganizationEditComponent.init (Some organization) (Some props.CurrentBuilding)
            Creating (false, organizationEditState), organizationEditCmd |> Cmd.map OrganizationEditMsg
        else
            Loading, getOrganizationCmd props.Identifier

    {
        CurrentUser = props.CurrentUser
        CurrentBuilding = props.CurrentBuilding
        OrganizationId = props.Identifier
        State = state
        NotifyCreated = props.NotifyCreated
        NotifyEdited = props.NotifyEdited
    }, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | View organization ->
        match organization with
        | Some organization ->
            { model with State = Viewing organization }, Cmd.none
        | None ->
            { model with State = OrganizationNotFound }, Cmd.none
    | Edit organization ->
        let organizationEditState, organizationEditCmd = OrganizationEditComponent.init (Some organization) (Some model.CurrentBuilding)
        { model with State = Editing (false, organizationEditState) }, organizationEditCmd |> Cmd.map OrganizationEditMsg
    | OrganizationEditMsg componentMsg ->
        let updateComponentState s (isSaving, componentState) =
            let newComponentState, newComponentCmd = OrganizationEditComponent.update componentMsg componentState
            { model with State = s (isSaving, newComponentState) }, 
            newComponentCmd |> Cmd.map OrganizationEditMsg


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
            match ValidatedOrganization.Validate componentState.Organization with
            | Ok organization ->
                let cmd = 
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().UpdateOrganization)
                        organization
                        (fun result -> result |> Result.map (fun _ -> componentState.Organization) |> ProcessUpdateResult)
                        RemotingError
                { model with State = Editing (true, componentState) }, cmd
            | Error e ->
                printf "Errors: %A" e
                { model with State = Editing (false, { componentState with Errors = e }) }, Cmd.none
        | Creating (_, componentState) ->
            match ValidatedOrganization.Validate componentState.Organization with
            | Ok organization ->
                let cmd =
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().CreateOrganization)
                        organization
                        (fun result -> result |> Result.map (fun _ -> componentState.Organization) |> ProcessCreateResult)
                        RemotingError
                { model with State = Creating(true, componentState) }, cmd
            | Error e ->
                printf "Errors: %A" e
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
    | OrganizationNotFound -> div [] [ str "De door u gekozen organisatie werd niet gevonden in de databank..." ]
    | State.RemotingError _ -> div [] [ str "Er is iets misgelopen bij het ophalen van de gegevens, gelieve de pagina te verversen" ]
    | Editing (isSaving, editState)
    | Creating (isSaving, editState) ->
        if isSaving 
        then
            div [] [ str "De organisatie wordt bewaard" ]
        else
            div [] [
                OrganizationEditComponent.view editState (OrganizationEditMsg >> dispatch)

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
            OrganizationViewComponent.render {| Organization = detail |}
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
    React.elmishComponent ("OrganizationDetails", init props, update, view, string props.Identifier)