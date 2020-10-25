module Client.Organizations.OrganizationTypesPage

open System
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Read
open Shared.Write
open Shared.Remoting

open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Library

type OrganizationTypesPageProps = {| CurrentUser: User |}

type State = {
    CurrentUser: User
    OrganizationTypes: (OrganizationType * EditingState option) list
}
and EditingState = 
    {
        IsNew: bool
        IsSaving: bool
        OrganizationType: OrganizationType
        Errors: (string * string) list
    }
    static member Init (isNew: bool) (organizationType: OrganizationType) = {
        IsNew = isNew
        IsSaving = false
        OrganizationType = organizationType
        Errors = []
    }

type Message =
    | Loaded of OrganizationType list
    | RemotingError of exn
    | Edit of Guid
    | SaveEdit of OrganizationType
    | FinishedEditing of OrganizationType
    | EditingFailed of OrganizationType * SaveOrganizationTypeError
    | CancelEdit of Guid

    | CreateOrganizationType
    | SaveCreation of OrganizationType
    | FinishedCreating of OrganizationType
    | CreationFailed of OrganizationType * SaveOrganizationTypeError
    | Delete of Guid
    | CancelCreation of Guid
    | EditOrganizationTypeName of (Guid * string)

let init (props: OrganizationTypesPageProps) =
    let state = {
        CurrentUser = props.CurrentUser
        OrganizationTypes = []
    }

    let cmd =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).GetOrganizationTypes ()
            Loaded
            RemotingError

    state, cmd

let update (message: Message) (state: State): State * Cmd<Message> =
    let changeOrganizationType (orgTypeId: Guid) (changeOrganizationType: OrganizationType -> OrganizationType) (changeEditingState: EditingState -> EditingState option): State =
        let newOrganizationTypes = 
            state.OrganizationTypes 
            |> List.map (fun (orgType, editingState) -> 
                if orgType.OrganizationTypeId = orgTypeId 
                then changeOrganizationType orgType, (editingState |> Option.bind changeEditingState) else orgType, editingState)
        { state with OrganizationTypes = newOrganizationTypes }

    match message with
    | Loaded organizationTypes ->
        { state with OrganizationTypes = organizationTypes |> List.map (fun li -> li, None) }, Cmd.none
    | RemotingError e ->
        state, showGenericErrorModalCmd e
    | Edit typeId ->
        let newOrgTypes = 
            state.OrganizationTypes 
            |> List.map (fun (li, editing) -> if li.OrganizationTypeId = typeId then li, Some (EditingState.Init false li) else li, editing)
        { state with OrganizationTypes = newOrgTypes }, Cmd.none
    | SaveEdit organizationType ->
        match ValidatedOrganizationType.Validate (organizationType) with
        | Ok _ ->
            let cmd = 
                Cmd.OfAsync.either
                    (Client.Remoting.getRemotingApi()).UpdateOrganizationType organizationType
                    (fun result -> match result with | Ok _ -> FinishedEditing organizationType | Error editingError -> EditingFailed (organizationType, editingError))
                    RemotingError

            let newState = changeOrganizationType (organizationType.OrganizationTypeId) id (fun s -> Some { s with IsSaving = true })
            newState, cmd
        | Error errors ->
            let newState = changeOrganizationType (organizationType.OrganizationTypeId) id (fun s -> Some { s with Errors = errors })
            newState, Cmd.none
    | SaveCreation organizationType ->
        match ValidatedOrganizationType.Validate (organizationType) with
        | Ok _ ->
            let cmd =
                Cmd.OfAsync.either
                    (Client.Remoting.getRemotingApi()).CreateOrganizationType organizationType
                    (fun result -> match result with | Ok _ -> FinishedCreating organizationType | Error creationError -> CreationFailed (organizationType, creationError))
                    RemotingError
            let newState = changeOrganizationType (organizationType.OrganizationTypeId) id (fun s -> Some { s with IsSaving = true })
            newState, cmd
        | Error errors ->
            let newState = changeOrganizationType (organizationType.OrganizationTypeId) id (fun s -> Some { s with Errors = errors })
            newState, Cmd.none
    | CreateOrganizationType ->
        let li = OrganizationType.Init ()
        let organizationType = li, Some (EditingState.Init true li)
        { state with OrganizationTypes = state.OrganizationTypes @ [ organizationType ] }, Cmd.none
    | CancelCreation orgTypeId ->
        let newOrganizationTypes = 
            state.OrganizationTypes 
            |> List.filter (fun (orgType, _) -> orgType.OrganizationTypeId <> orgTypeId)
        { state with OrganizationTypes = newOrganizationTypes }, Cmd.none
    | Delete orgTypeId ->
        let cmd =
            Cmd.OfAsync.attempt
                (Client.Remoting.getRemotingApi()).DeleteOrganizationType orgTypeId
                RemotingError
        let newState = { state with OrganizationTypes = state.OrganizationTypes |> List.filter (fun (orgType, _) -> orgType.OrganizationTypeId <> orgTypeId) }
        newState, cmd
    | CreationFailed (organizationType, e) ->
        match e with
        | SaveOrganizationTypeError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om een leverancier type aan te maken"
        | SaveOrganizationTypeError.Validation errors ->
            changeOrganizationType (organizationType.OrganizationTypeId) id (fun s -> Some { s with Errors = errors }), Cmd.none
        | SaveOrganizationTypeError.NotFound ->
            state, showErrorToastCmd "Het leverancier type werd niet gevonden in de databank"
    | EditingFailed (organizationType, e) ->
        match e with
        | SaveOrganizationTypeError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om dit leverancier type te updaten"
        | SaveOrganizationTypeError.NotFound ->
            state, showErrorToastCmd "Het leverancier type werd niet gevonden in de databank"
        | SaveOrganizationTypeError.Validation errors ->
            changeOrganizationType (organizationType.OrganizationTypeId) id (fun s -> Some { s with Errors = errors }), Cmd.none
    | CancelEdit orgTypeId ->
        let newState = changeOrganizationType (orgTypeId) id (fun _ -> None)
        newState, Cmd.none       
    | FinishedEditing orgType
    | FinishedCreating orgType ->
        let newState = changeOrganizationType (orgType.OrganizationTypeId) (fun _ -> orgType) (fun _ -> None)
        newState, Cmd.none
    | EditOrganizationTypeName (orgTypeId, newName) ->
        let newState = changeOrganizationType (orgTypeId) id (fun s -> Some { s with OrganizationType = { s.OrganizationType with Name = newName }; Errors = [] })
        newState, Cmd.none
            
let view (state: State) (dispatch: Message -> unit) =
    let toRow (organizationType: OrganizationType, editingState: EditingState option) =
        match editingState with
        | None ->
            tr [] [
                td [] [ str organizationType.Name ]
                td [] [ 
                    a [ 
                        Class "pointer"
                        OnClick (fun _ -> Edit organizationType.OrganizationTypeId |> dispatch) 
                    ] [
                        i [ classes [ FontAwesome.fa; FontAwesome.faEdit ] ] []
                    ]
                ]
                td [] [
                    a [
                        Class "pointer"
                        OnClick (fun _ -> Delete organizationType.OrganizationTypeId |> dispatch)
                    ] [
                        i [ classes [ FontAwesome.fa; FontAwesome.faTrashAlt ] ] []
                    ]
                ]
            ]
        | Some editingState ->
            if editingState.IsSaving 
            then
                tr [] [
                    td [ ColSpan 3 ] [ str "Bezig met bewaren" ]
                ]
            else
                tr [] [
                    td [] [
                        form [ 
                            Id (string editingState.OrganizationType.OrganizationTypeId)
                            OnSubmit (fun _ -> 
                                if editingState.IsNew 
                                then (SaveCreation editingState.OrganizationType |> dispatch) 
                                else (SaveEdit editingState.OrganizationType |> dispatch)
                            ) 
                        ] [
                        ]
                        formGroup [
                            Input [
                                Form (string editingState.OrganizationType.OrganizationTypeId)
                                Type "Text"
                                MaxLength 255.0
                                Helpers.valueOrDefault editingState.OrganizationType.Name
                                OnChange (fun e -> EditOrganizationTypeName (organizationType.OrganizationTypeId, e.Value) |> dispatch)
                            ]
                        ]                        

                    ]
                    td [] [ 
                        a [ 
                            yield Class "pointer"
                            yield
                                if editingState.IsNew 
                                then
                                    OnClick (fun _ -> SaveCreation editingState.OrganizationType |> dispatch)
                                else
                                    OnClick (fun _ -> SaveEdit editingState.OrganizationType |> dispatch) 
                        ] [
                            i [ classes [ FontAwesome.fa; FontAwesome.faCheck ] ] []
                        ]
                    ]
                    td [] [
                        a [
                            yield Class "pointer"
                            yield
                                if editingState.IsNew 
                                then
                                    OnClick (fun _ -> CancelCreation organizationType.OrganizationTypeId |> dispatch)
                                else
                                    OnClick (fun _ -> CancelEdit organizationType.OrganizationTypeId |> dispatch) 
                        ] [
                            i [ classes [ FontAwesome.fa; FontAwesome.faUndoAlt ] ] []
                        ]
                    ]
                ]

    div [] [
        table [ classes [ Bootstrap.table; Bootstrap.tableStriped; Bootstrap.tableHover ] ] [
            thead [] [
                tr [] [
                    th [] [ str "Naam" ]
                    th [] []
                    th [] []
                ]
            ]
            tbody [] [
                yield! state.OrganizationTypes |> List.map toRow
            ]
        ]
        div [ classes [ Bootstrap.card; Bootstrap.bgLight ] ] [
            div [ Class Bootstrap.cardBody ] [
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnSm; Bootstrap.btnSuccess ]
                    OnClick (fun _ -> CreateOrganizationType |> dispatch) 
                ] [
                    i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                    str " "
                    str "Type aanmaken"
                ]
            ]
        ]
    ]

let render (props: OrganizationTypesPageProps) =
    React.elmishComponent ("OrganizationTypes", init props, update, view)