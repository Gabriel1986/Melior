module Client.Financial.Settings

open System
open Elmish
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents
open Shared.Read
open Shared.Remoting
open Shared.Library
open Client
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Library
open Client.Components
open Client.Components.BasicModal

type CreateOrUpdate'<'T> =
    | Create' of 'T
    | Update' of 'T

type State = {
    CurrentUser: User
    CurrentBuildingId: Guid
    IsLoadingFinancialYears: bool
    IsLoadingFinancialCategories: bool
    FinancialYears: FinancialYear list
    FinancialCategories: FinancialCategory list
    ShowEndedFinancialYears: bool
    FinancialYearIsClosing: Set<Guid>
    FinancialYearModalState: FinancialYearModalState option
    FinancialCategoryModalState: FinancialCategoryModalState option
}

and FinancialYearModalState =
    {
        IsSaving: bool
        ComponentState: FinancialYearEditComponent.State
    }
    static member Init componentState = {
        IsSaving = false
        ComponentState = componentState
    }

//TODO: refactor.
and FinancialCategoryModalState = 
    {
        IsSaving: bool
        FinancialCategory: CreateOrUpdate'<FinancialCategory>
        Errors: (string * string) list
    }
    static member Init (createOrUpdate) = {
        IsSaving = false
        FinancialCategory = createOrUpdate
        Errors = []
    }

type Msg =
    | FinancialYearsLoaded of FinancialYear list
    | FinancialCategoriesLoaded of FinancialCategory list
    | RemotingError of exn

    | CloseFinancialYear of FinancialYear
    | ConfirmCloseFinancialYear of FinancialYear
    | CancelCloseFinancialYear
    | FinancialYearIsClosed of Result<Guid, SaveFinancialYearError>
    | ToggleShowEndedFinancialYears

    | OpenCreateFinancialCategory
    | CreateFinancialCategory of FinancialCategory
    | FinancialCategoryCreated of Result<FinancialCategory, SaveFinancialCategoryError>

    | OpenUpdateFinancialCategory of FinancialCategory
    | UpdateFinancialCategory of FinancialCategory
    | FinancialCategoryUpdated of Result<FinancialCategory, SaveFinancialCategoryError>

    | OpenCreateFinancialYear
    | CreateFinancialYear of FinancialYear
    | FinancialYearCreated of Result<FinancialYear, SaveFinancialYearError>
    
    | OpenUpdateFinancialYear of FinancialYear
    | UpdateFinancialYear of FinancialYear
    | FinancialYearUpdated of Result<FinancialYear, SaveFinancialYearError>
    | CloseFinancialYearEditor
    
    | DeleteFinancialCategory of FinancialCategory
    | ConfirmDeleteFinancialCategory of FinancialCategory
    | CancelDeleteFinancialCategory
    | FinancialCategoryDeleted of Result<FinancialCategory, DeleteFinancialCategoryError>

    //Components
    | FinancialYearEditComponentMsg of FinancialYearEditComponent.Message

type SettingsPageProps = 
    {| 
        CurrentUser: User
        CurrentBuildingId: BuildingId
    |}

let init (props: SettingsPageProps) =
    {
        CurrentUser = props.CurrentUser
        CurrentBuildingId = props.CurrentBuildingId
        IsLoadingFinancialCategories = true
        IsLoadingFinancialYears = true
        FinancialYears = []
        FinancialCategories = []
        ShowEndedFinancialYears = false
        FinancialYearIsClosing = Set.empty
        FinancialCategoryModalState = None
        FinancialYearModalState = None
    },
    Cmd.batch [
        Cmd.OfAsync.either (Remoting.getRemotingApi()).GetFinancialCategories props.CurrentBuildingId FinancialCategoriesLoaded RemotingError
        Cmd.OfAsync.either (Remoting.getRemotingApi()).GetFinancialYears props.CurrentBuildingId FinancialYearsLoaded RemotingError
    ]

module private Server =
    let createFinancialYear (year: FinancialYear) =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).CreateFinancialYear year
            (fun result -> FinancialYearCreated (result |> Result.map (fun () -> year)))
            RemotingError

    let updateFinancialYear (year: FinancialYear) =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).UpdateFinancialYear year
            (fun result -> FinancialYearUpdated (result |> Result.map (fun () -> year)))
            RemotingError

    let closeFinancialYear (buildingId: Guid) (financialYearId: Guid) =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).CloseFinancialYear (buildingId, financialYearId)
            (fun result -> FinancialYearIsClosed (result |> Result.map (fun () -> financialYearId)))
            RemotingError

    let createFinancialCategory (category: FinancialCategory) =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).CreateFinancialCategory category
            (fun result -> FinancialCategoryCreated (result |> Result.map (fun () -> category)))
            RemotingError

    let updateFinancialCategory (category: FinancialCategory) =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).UpdateFinancialCategory category
            (fun result -> FinancialCategoryUpdated (result |> Result.map (fun () -> category)))
            RemotingError

    let deleteFinancialCategory (category: FinancialCategory) =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).DeleteFinancialCategory (category.BuildingId.Value, category.FinancialCategoryId)
            (fun result -> FinancialCategoryDeleted (result |> Result.map (fun () -> category)))
            RemotingError

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let updateFinancialYearModalState (update: FinancialYearModalState option -> FinancialYearModalState option) (currentState: State): State =
        { currentState with FinancialYearModalState = update currentState.FinancialYearModalState }
    
    let updateFinancialCategoryModalState (update: FinancialCategoryModalState option -> FinancialCategoryModalState option) (currentState: State): State =
        { currentState with FinancialCategoryModalState = update currentState.FinancialCategoryModalState }

    match msg with
    | FinancialYearsLoaded financialYears ->
        { state with FinancialYears = financialYears; IsLoadingFinancialYears = false }, Cmd.none
    | FinancialCategoriesLoaded financialCategories ->
        { state with FinancialCategories = financialCategories; IsLoadingFinancialCategories = false }, Cmd.none
    | CloseFinancialYear financialYear ->
        state,
        showConfirmationModal (
            (sprintf "Boekjaar %s beëindigen?" financialYear.Code),
            "Hierdoor worden alle financiële verrichtingen die gekoppeld zijn aan het boekjaar afgesloten (u kan ze niet meer aanpassen)",
            (fun () -> ConfirmCloseFinancialYear financialYear),
            (fun () -> CancelCloseFinancialYear))
    | CancelCloseFinancialYear ->
        state, Cmd.none
    | ConfirmCloseFinancialYear financialYear ->
        let newFinancialYears =
            state.FinancialYears 
            |> List.map (fun year -> if year.FinancialYearId = financialYear.FinancialYearId then { year with IsClosed = true } else year)

        {
            state with
                FinancialYears = newFinancialYears
                FinancialYearIsClosing = state.FinancialYearIsClosing |> Set.add financialYear.FinancialYearId
        }, Server.closeFinancialYear state.CurrentBuildingId financialYear.FinancialYearId
    | FinancialYearIsClosed (Ok financialYearId) ->
        { state with FinancialYearIsClosing = state.FinancialYearIsClosing |> Set.filter (fun yearId -> yearId <> financialYearId) }
        , Cmd.none
    | FinancialYearIsClosed (Error e) ->
        state, showGenericErrorModalCmd (exn (string e))
    | ToggleShowEndedFinancialYears ->
        { state with ShowEndedFinancialYears = not state.ShowEndedFinancialYears }, Cmd.none
    | OpenCreateFinancialYear ->
        let componentState, componentCmd = FinancialYearEditComponent.initCreate (FinancialYear.Init state.CurrentBuildingId) 

        state |> updateFinancialYearModalState (fun _ -> Some (FinancialYearModalState.Init componentState))
        , componentCmd |> Cmd.map FinancialYearEditComponentMsg
    | CreateFinancialYear year ->
        state |> updateFinancialYearModalState (Option.map (fun s -> { s with IsSaving = true }))
        , Server.createFinancialYear year
    | FinancialYearCreated (Ok(year)) ->
        //TODO: validate here in stead of on the server.
        let updatedYears = year::state.FinancialYears
        { state with FinancialYears = updatedYears }
        |> updateFinancialYearModalState (fun _ -> None)
        , showSuccessToastCmd "Het boekjaar is aangemaakt"
    | OpenUpdateFinancialYear year ->
        let componentState, componentCmd = FinancialYearEditComponent.initUpdate(year) 

        state
        |> updateFinancialYearModalState (fun _ -> Some (FinancialYearModalState.Init componentState))
        , componentCmd |> Cmd.map FinancialYearEditComponentMsg
    | UpdateFinancialYear year ->
        state
        |> updateFinancialYearModalState (Option.map (fun s -> { s with IsSaving = true }))
        , Server.updateFinancialYear year
    | FinancialYearUpdated (Ok(year)) ->
        //TODO: validate here in stead of on the server.
        let updatedCategories = 
            state.FinancialYears 
            |> List.map (fun fc -> if fc.FinancialYearId = year.FinancialYearId then year else fc)
        { state with FinancialYears = updatedCategories }
        |> updateFinancialYearModalState (fun _ -> None)
        , showSuccessToastCmd "Het boekjaar is gewijzigd"
    | CloseFinancialYearEditor ->
        state |> updateFinancialCategoryModalState (fun _ -> None)
        , Cmd.none
    | OpenCreateFinancialCategory ->
        let newCategory = FinancialCategory.Init state.CurrentBuildingId
        state |> updateFinancialCategoryModalState (fun _ -> Some (FinancialCategoryModalState.Init (Create' newCategory)))
        , Cmd.none
    | CreateFinancialCategory category ->
        state |> updateFinancialCategoryModalState (Option.map (fun s -> { s with FinancialCategory = Create' category; IsSaving = true }))
        , Server.createFinancialCategory category
    | FinancialCategoryCreated (Ok(category)) ->
        let updatedCategories = category::state.FinancialCategories
        { state with FinancialCategories = updatedCategories }
        |> updateFinancialCategoryModalState (fun _ -> None)
        , showSuccessToastCmd "De boekhoudkundige rekening is aangemaakt"
    | OpenUpdateFinancialCategory category ->
        state
        |> updateFinancialCategoryModalState (fun _ -> Some (FinancialCategoryModalState.Init (Update' category)))
        , Cmd.none
    | UpdateFinancialCategory category ->
        state
        |> updateFinancialCategoryModalState (Option.map (fun s -> { s with FinancialCategory = Update' category; IsSaving = true }))
        , Server.updateFinancialCategory category
    | FinancialCategoryUpdated (Ok(category)) ->
        let updatedCategories = 
            state.FinancialCategories 
            |> List.map (fun fc -> if fc.FinancialCategoryId = category.FinancialCategoryId then category else fc)
        { state with FinancialCategories = updatedCategories }
        |> updateFinancialCategoryModalState (fun _ -> None)
        , showSuccessToastCmd "De boekhoudkundige rekening is gewijzigd"
    | DeleteFinancialCategory category ->
        state, 
        showConfirmationModal (
            (sprintf "Boekhoudkundige rekening %s verwijderen?" category.Code), 
            "",
            (fun () -> ConfirmDeleteFinancialCategory category), 
            (fun () -> CancelDeleteFinancialCategory))
    | ConfirmDeleteFinancialCategory category ->
        let newCategories = 
            state.FinancialCategories 
            |> List.filter (fun cat -> cat.FinancialCategoryId <> category.FinancialCategoryId)
        { state with FinancialCategories = newCategories }, Server.deleteFinancialCategory category
    | FinancialCategoryDeleted (Ok(_category)) ->
        state, Cmd.none
    | CancelDeleteFinancialCategory ->
        state, Cmd.none

    | FinancialYearCreated (Error e)
    | FinancialYearUpdated (Error e) ->
        match e with
        | SaveFinancialYearError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om dit boekjaar te bewaren"
        | SaveFinancialYearError.NotFound ->
            state, showErrorToastCmd "Het boekjaar werd niet gevonden in de databank"
        | SaveFinancialYearError.Validation errors ->
            state 
            |> updateFinancialYearModalState (Option.map (fun s -> { s with IsSaving = false; ComponentState = { s.ComponentState with Errors = errors } }))
            , Cmd.none
    | FinancialCategoryCreated (Error e)
    | FinancialCategoryUpdated (Error e) ->
        match e with
        | SaveFinancialCategoryError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om deze boekhoudkundige rekening te bewaren"
        | SaveFinancialCategoryError.NotFound ->
            state, showErrorToastCmd "De boekhoudkundige rekening werd niet gevonden in de databank"
        | SaveFinancialCategoryError.Validation errors ->
            state |> updateFinancialCategoryModalState (Option.map (fun s -> { s with IsSaving = false; Errors = errors }))
            , Cmd.none
    | FinancialCategoryDeleted (Error(errors)) ->
        match errors with
        | DeleteFinancialCategoryError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om deze boekhoudkundige rekening te verwijderen"
        | DeleteFinancialCategoryError.NotFound ->
            state, showErrorToastCmd "De boekhoudkundige rekening werd niet gevonden in de databank"
    | RemotingError error ->
        state, showGenericErrorModalCmd error

    | FinancialYearEditComponentMsg msg ->
        match state.FinancialYearModalState with
        | Some modalState ->
            let updatedComponentState, updatedComponentCmd = FinancialYearEditComponent.update msg modalState.ComponentState

            state
            |> updateFinancialYearModalState (Option.map (fun l -> { l with ComponentState  = updatedComponentState }))
            , updatedComponentCmd |> Cmd.map FinancialYearEditComponentMsg
        | None ->
            state, Cmd.none

let renderYearHeader () =
    div [ Class Bootstrap.row ] [
        div [ Class Bootstrap.col3 ] [ str "Code" ]
        div [ Class Bootstrap.col3 ] [ str "Begindatum" ]
        div [ Class Bootstrap.col3 ] [ str "Einddatum" ]
        div [ Class Bootstrap.col3 ] [ str "" ]
    ]

let renderYear (year: FinancialYear) (state: State) (dispatch: Msg -> unit) =
    div [ Class Bootstrap.row ] [
        div [ Class Bootstrap.col ] [
            readonlyFormElement "Code" year.Code
            readonlyFormElement "Startdatum" (year.StartDate.ToString("dd/MM/yyyy"))
            readonlyFormElement "Einddatum" (year.EndDate.ToString("dd/MM/yyyy"))
        ]
        div [] [            
            //TODO: When the year is closed, flatten and copy invoices and deposits
            //if state.FinancialYearIsClosing.Contains(year.FinancialYearId) then
            //    button [ classes [ Bootstrap.btn; Bootstrap.btnPrimary ]; Disabled true ] [ 
            //        str "Wordt afgesloten" 
            //    ]
            //elif year.IsClosed then
            //    button [ classes [ Bootstrap.btn; Bootstrap.btnSecondary ]; Disabled true ] [ 
            //        str "Afgesloten" 
            //    ]
            //else
            //    button [ classes [ Bootstrap.btn; Bootstrap.btnPrimary ]; OnClick (fun _ -> CloseFinancialYear year |> dispatch) ] [ 
            //        str "Afsluiten" 
            //    ]
        ]
    ]

let private renderFinancialYearModal (state: State) (dispatch: Msg -> unit) =
    BasicModal.render 
        {|                
            ModalProps = [
                ModalProp.OnDismiss (fun _ -> CloseFinancialYearEditor |> dispatch)
                ModalProp.IsOpen state.FinancialYearModalState.IsSome
                ModalProp.Header [
                    HeaderProp.HasDismissButton true
                    HeaderProp.Title "Boekjaar"
                ]
                ModalProp.Body [
                    match state.FinancialYearModalState with
                    | Some financialYearModalState ->
                        if financialYearModalState.IsSaving then
                            div [] [ str "Uw wijzigingen worden bewaard..." ]
                        else
                            FinancialYearEditComponent.view (financialYearModalState.ComponentState) (FinancialYearEditComponentMsg >> dispatch)
                    | None ->
                        div [] []
                ]
                ModalProp.Footer [
                    FooterProp.Buttons [
                        match state.FinancialYearModalState with
                        | Some modalState ->
                            button [
                                classes [ Bootstrap.btn; Bootstrap.btnPrimary ] 
                                OnClick (fun _ -> 
                                    let componentState = modalState.ComponentState
                                    match componentState.CreateOrUpdate with
                                    | Create -> CreateFinancialYear componentState.FinancialYear
                                    | Update -> UpdateFinancialYear componentState.FinancialYear 
                                    |> dispatch)
                            ] [ str "Bewaren" ]
                        | None ->
                            yield! []
                    ]
                    FooterProp.ShowDismissButton (Some "Annuleren")
                ]
            ] 
        |}

let view (state: State) (dispatch: Msg -> unit) =
    let shownFinancialYears =
        let visibleFinancialYears =
            state.FinancialYears
            |> List.sortByDescending (fun year -> year.StartDate)

        if state.ShowEndedFinancialYears then
            visibleFinancialYears
        else
            visibleFinancialYears 
            |> List.filter (fun year -> not year.IsClosed)

    [
        div [ Class Bootstrap.row ] [
            fieldset [ Class Bootstrap.col ] [
                legend [] [ h4 [] [ str "Boekjaren (voorlopig wordt maar 1 jaar ondersteund)" ] ]
                div [] [
                    yield! shownFinancialYears |> List.map (fun year -> renderYear year state dispatch)
                ]
                //TODO: add ability to have multiple financial years
                if shownFinancialYears.Length = 0 then
                    div [ classes [ Bootstrap.card; Bootstrap.bgLight ] ] [
                        div [ Class Bootstrap.cardBody ] [
                            button [
                                classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
                                OnClick (fun _ -> OpenCreateFinancialYear |> dispatch)
                            ] [
                                i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                                str " "
                                str "Nieuw boekjaar beginnen"
                            ]
                            //str " "
                            //button [
                            //    classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]
                            //    OnClick (fun _ -> ToggleShowEndedFinancialYears |> dispatch)
                            //] [
                            //    if state.ShowEndedFinancialYears
                            //    then 
                            //        [ 
                            //            i [ classes [ FontAwesome.fa; FontAwesome.faEyeSlash ] ] []
                            //            str " "
                            //            str "Alle boekjaren verbergen" 
                            //        ] |> React.fragment
                            //    else 
                            //        [ 
                            //            i [ classes [ FontAwesome.fa; FontAwesome.faEye ] ] []
                            //            str " "
                            //            str "Alle boekjaren tonen" 
                            //        ] |> React.fragment
                            //]
                        ]
                    ]
                else
                    null
            ]
        ]

        div [ Class Bootstrap.row ] [
            fieldset [ Class Bootstrap.col ] [
                legend [] [ h4 [] [ str "Boekhoudkundige rekeningen (voorlopig niet wijzigbaar)" ] ]
                let allCategories = state.FinancialCategories |> List.sortBy (fun fc -> fc.Code)
                let parents = allCategories |> List.filter (fun fc -> fc.Code.Length = 3 || fc.Code = "400000")
                let rec renderChildren parentCode level =
                    allCategories 
                    |> List.filter (fun fc -> 
                        if parentCode = "400000"
                        then fc.Code.StartsWith "40" && fc.Code <> "400000"
                        else fc.Code.StartsWith(parentCode) && fc.Code.Length = parentCode.Length + 1)
                    |> List.map (fun child -> 
                        [
                            li [] [ 
                                yield str (seq { for _ in 1..level do yield "" } |> String.JoinWith " ")
                                yield str (sprintf "%s - %s" child.Code child.Description)
                                //TODO.
                                //if (child.BuildingId.IsSome) then
                                //    yield! [
                                //        i [ classes [ FontAwesome.fa; FontAwesome.faEdit ]; OnClick (fun _ -> OpenUpdateFinancialCategory child |> dispatch) ] []
                                //        str " "
                                //        i [ classes [ FontAwesome.fa; FontAwesome.faTrashAlt ]; OnClick (fun _ -> DeleteFinancialCategory child |> dispatch) ] []
                                //    ]
                            ]
                            match renderChildren child.Code (level + 1) with
                            | [] -> null
                            | children -> ul [] [ yield! children ]
                        ] |> React.fragment)
            
                ul [] [
                    yield!
                        parents 
                        |> List.map (fun parent -> 
                            [
                                li [] [ str (sprintf "%s - %s" parent.Code parent.Description) ]
                                ul [] [ yield! renderChildren parent.Code 1 ]
                            ] |> React.fragment)
                ]
                //TODO
                //div [ classes [ Bootstrap.card; Bootstrap.bgLight ] ] [
                //    div [ Class Bootstrap.cardBody ] [
                //        button [ 
                //            classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
                //            OnClick (fun _ -> OpenCreateFinancialCategory |> dispatch)
                //        ] [
                //            i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                //            str " "
                //            str "Nieuwe boekhoudkundige rekening aanmaken"
                //        ]
                //    ]
                //]
            ]
        ]

        renderFinancialYearModal state dispatch
    ]
    |> React.fragment

let render (props: SettingsPageProps) =
    React.elmishComponent ("SettingsPage", init props, update, view)