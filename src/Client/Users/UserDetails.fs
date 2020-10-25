module Client.Users.UserDetails

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
    Detail: User
    CurrentUser: User
    State: State
    NotifyCreated: User -> unit
    NotifyEdited:  User -> unit
}
and State =
    | Loading
    | Viewing  of detail: User
    | Editing  of isSaving: bool * userEditState: UserEditComponent.State
    | Creating of isSaving: bool * userEditState: UserEditComponent.State
    | UserNotFound

type Msg =
    | UserEditMsg of UserEditComponent.Message
    | View of User option
    | Edit of User
    | RemotingError of exn
    | Save
    | ProcessCreateResult of Result<User, SaveUserError>
    | ProcessUpdateResult of Result<User, SaveUserError>

type CreateOrUpdate =
    | Create of userId: Guid
    | Update of User
    member me.UserId =
        match me with
        | Create userId -> userId
        | Update user -> user.UserId

type DetailsProps = {|
    CurrentUser: User
    CreateOrUpdate: CreateOrUpdate
    NotifyCreated: User -> unit
    NotifyEdited: User -> unit
|}

let init (props: DetailsProps): Model * Cmd<Msg> =
    let state, cmd, user =
        match props.CreateOrUpdate with
        | Create userId ->
            let user = { User.Init () with UserId = userId }
            let userEditState, userEditCmd = UserEditComponent.init user
            Creating (false, userEditState), userEditCmd |> Cmd.map UserEditMsg, user
        | Update user ->
            Viewing user, Cmd.none, user

    {
        CurrentUser = props.CurrentUser
        Detail = user
        State = state
        NotifyCreated = props.NotifyCreated
        NotifyEdited = props.NotifyEdited
    }, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let changeViewingState (change: User -> User) =
        match model.State with
        | Viewing user -> Viewing (change user)
        | other -> other

    let processSaveUserError =
        function
        | SaveUserError.AuthorizationError ->
            model, showErrorToastCmd "U heeft geen toestemming om dit gebouw te bewaren"
        | SaveUserError.NotFound ->
            model, showErrorToastCmd "Het gebouw werd niet gevonden in de databank"
        | SaveUserError.Validation errors ->
            match model.State with
            | Creating (_, componentState) ->
                { model with State = Creating (false, { componentState with Errors = errors }) }, Cmd.none                
            | Editing (_, componentState) ->
                { model with State = Editing (false, { componentState with Errors = errors }) }, Cmd.none
            | _ ->
                model, Cmd.none

    match msg with
    | View user ->
        match user with
        | Some user ->
            { model with State = Viewing user }, Cmd.none
        | None ->
            { model with State = UserNotFound }, Cmd.none
    | Edit user ->
        let userEditState, userEditCmd = UserEditComponent.init user
        { model with State = Editing (false, userEditState) }, userEditCmd |> Cmd.map UserEditMsg
    | UserEditMsg componentMsg ->
        let updateComponentState s (isSaving, componentState) =
            let newComponentState, newComponentCmd = UserEditComponent.update componentMsg componentState
            { model with State = s (isSaving, newComponentState) }, 
            newComponentCmd |> Cmd.map UserEditMsg
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
            match ValidatedUser.Validate componentState.User with
            | Ok _ ->
                let cmd = 
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().UpdateUser)
                        componentState.User
                        (fun result -> result |> Result.map (fun _ -> componentState.User) |> ProcessUpdateResult)
                        RemotingError
                { model with State = Editing (true, componentState) }, cmd
            | Error e ->
                printf "%A" e
                { model with State = Editing (false, { componentState with Errors = e }) }, Cmd.none
        | Creating (_, componentState) ->
            match ValidatedUser.Validate componentState.User with
            | Ok _ ->
                let cmd =
                    Cmd.OfAsync.either
                        (Remoting.getRemotingApi().CreateUser)
                        componentState.User
                        (fun result -> result |> Result.map (fun _ -> componentState.User) |> ProcessCreateResult)
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
            processSaveUserError e

    | ProcessUpdateResult result ->
        match result with
        | Ok result ->
            match model.State with
            | Editing _ -> model.NotifyEdited result
            | _ -> ()
            model, Cmd.none
        | Error e ->
            processSaveUserError e



let view (model: Model) (dispatch: Msg -> unit) =
    match model.State with
    | Loading ->  div [] [ str "Details worden geladen" ]
    | UserNotFound -> div [] [ str "De door u gekozen gebruiker werd niet gevonden in de databank..." ]
    | Editing (isSaving, editState)
    | Creating (isSaving, editState) ->
        if isSaving 
        then
            div [] [ str "De gebruiker wordt bewaard" ]
        else
            div [] [
                UserEditComponent.view editState (UserEditMsg >> dispatch)

                div [ classes [ Bootstrap.card; Bootstrap.bgLight; Bootstrap.mt5 ] ] [
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
            UserViewComponent.render {| User = detail |}
            div [ classes [ Bootstrap.card; Bootstrap.bgLight; Bootstrap.mt5 ] ] [
                div [ Class Bootstrap.cardBody ] [
                    yield
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
    React.elmishComponent ("UserDetails", init props, update, view, string props.CreateOrUpdate.UserId)