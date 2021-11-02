module Client.Financial.Deposits.DepositRequestDetails

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
open DepositTypes

type Model = {
    DepositRequestId: Guid
    CurrentBuilding: BuildingListItem
    CurrentUser: User
    State: State
    FinancialCategories: FinancialCategory list
    NotifyCreated: DepositRequest -> unit
    NotifyEdited:  DepositRequest -> unit
    DepositModalIsOpenOn: DepositTypes.CreateOrUpdate option
}
and State =
    | Loading
    | Viewing  of detail: DepositRequest
    | Editing  of isSaving: bool * depositRequestEditState: DepositRequestEditComponent.State
    | Creating of isSaving: bool * depositRequestEditState: DepositRequestEditComponent.State
    | InvoiceNotFound

type Msg =
    | InvoiceEditMsg of DepositRequestEditComponent.Message
    | View of DepositRequest option
    | Edit of DepositRequest
    | RemotingError of exn
    | Save
    | ProcessCreateResult of Result<DepositRequest, SaveDepositRequestError>
    | ProcessUpdateResult of Result<DepositRequest, SaveDepositRequestError>

    | AddDeposit
    | EditDeposit of Deposit
    | RemoveDeposit of Deposit
    | ConfirmRemoveDeposit of Deposit
    | DepositWasCanceled
    | DepositWasAdded of DepositTypes.CreatedOrUpdated
    | DepositWasRemoved of Deposit
    | FinancialCategoriesLoaded of FinancialCategory list
    | NoOp

type DetailsProps = {|
    CurrentUser: User
    CurrentBuilding: BuildingListItem
    Identifier: Guid
    IsNew: bool
    NotifyCreated: DepositRequest -> unit
    NotifyEdited: DepositRequest -> unit
|}

module Server =
    let getDepositRequestCmd identifier =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetDepositRequest)
            identifier
            View
            RemotingError

    let removeDeposit (buildingId: BuildingId, deposit: Deposit) =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().DeleteDeposit)
            (buildingId, deposit.DepositId)
            (fun _ -> DepositWasRemoved deposit)
            RemotingError

    let getFinancialCategories (buildingId: BuildingId) =
        Cmd.OfAsync.either 
            (Remoting.getRemotingApi().GetFinancialCategories) 
                buildingId 
                FinancialCategoriesLoaded 
                RemotingError

let init (props: DetailsProps): Model * Cmd<Msg> =
    let state, cmd =
        if props.IsNew then
            let invoiceEditState, invoiceEditCmd = 
                DepositRequestEditComponent.init {| CurrentBuilding = props.CurrentBuilding; DepositRequest = None |}
            Creating (false, invoiceEditState), invoiceEditCmd |> Cmd.map InvoiceEditMsg
        else
            Loading, Server.getDepositRequestCmd props.Identifier

    {
        CurrentUser = props.CurrentUser
        CurrentBuilding = props.CurrentBuilding
        DepositRequestId = props.Identifier
        State = state
        NotifyCreated = props.NotifyCreated
        NotifyEdited = props.NotifyEdited
        FinancialCategories = []
        DepositModalIsOpenOn = None
    }, Cmd.batch [ cmd; Server.getFinancialCategories (props.CurrentBuilding.BuildingId) ]

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let processSaveInvoiceError =
        function
        | SaveDepositRequestError.AuthorizationError ->
            model, showErrorToastCmd "U heeft geen toestemming om deze provisie te bewaren"
        | SaveDepositRequestError.NotFound ->
            model, showErrorToastCmd "De provisie werd niet gevonden in de databank"
        | SaveDepositRequestError.Validation errors ->
            match model.State with
            | Creating (_, componentState) ->
                { model with State = Creating (false, { componentState with Errors = errors }) }, Cmd.none
            | Editing (_, componentState) ->
                { model with State = Editing (false, { componentState with Errors = errors }) }, Cmd.none
            | _ ->
                model, Cmd.none

    match msg with
    | View request ->
        match request with
        | Some request ->
            { model with State = Viewing request }, Cmd.none
        | None ->
            { model with State = InvoiceNotFound }, Cmd.none
    | Edit request ->
        let invoiceEditState, invoiceEditCmd = DepositRequestEditComponent.init {| DepositRequest = Some request; CurrentBuilding = model.CurrentBuilding |}
        { model with State = Editing (false, invoiceEditState) }, invoiceEditCmd |> Cmd.map InvoiceEditMsg    
    | InvoiceEditMsg componentMsg ->
        let updateComponentState s (isSaving, componentState) =
            let newComponentState, newComponentCmd = DepositRequestEditComponent.update componentMsg componentState
            { model with State = s (isSaving, newComponentState) }, 
            newComponentCmd |> Cmd.map InvoiceEditMsg


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
            match componentState.Request.Validate () with
            | Ok request ->
                match ValidatedDepositRequest.Validate request with
                | Ok _ ->
                    let cmd = 
                        Cmd.OfAsync.either
                            (Remoting.getRemotingApi().UpdateDepositRequest)
                            request
                            (fun result -> result |> Result.map (fun _ -> request) |> ProcessUpdateResult)
                            RemotingError
                    { model with State = Editing (true, componentState) }, cmd
                | Error e ->
                    { model with State = Editing (false, { componentState with Errors = e }) }, Cmd.none
            | Error e ->
                { model with State = Editing (false, { componentState with Errors = e }) }, Cmd.none
        | Creating (_, componentState) ->
            match componentState.Request.Validate () with
            | Ok request ->
                match ValidatedDepositRequest.Validate request with
                | Ok _ ->
                    let cmd = 
                        Cmd.OfAsync.either
                            (Remoting.getRemotingApi().CreateDepositRequest)
                            request
                            (fun result -> result |> Result.map (fun _ -> request) |> ProcessCreateResult)
                            RemotingError
                    { model with State = Creating (true, componentState) }, cmd
                | Error e ->
                    { model with State = Creating (false, { componentState with Errors = e }) }, Cmd.none
            | Error e ->
                { model with State = Creating (false, { componentState with Errors = e }) }, Cmd.none
        | _ ->
            //Do nothing, unexpected message O_o
            model, Cmd.none
    | RemotingError e ->
        model, showGenericErrorModalCmd e
    | FinancialCategoriesLoaded financialCategories ->
        { model with FinancialCategories = financialCategories }, Cmd.none
    | ProcessCreateResult result ->
        match result with
        | Ok result ->
            match model.State with
            | Creating _ -> model.NotifyCreated result
            | _ -> ()
            { model with State = Viewing result }, Cmd.none
        | Error e ->
            processSaveInvoiceError e
    | ProcessUpdateResult result ->
        match result with
        | Ok result ->
            match model.State with
            | Editing _ -> model.NotifyEdited result
            | _ -> ()
            { model with State = Viewing result }, Cmd.none
        | Error e ->
            processSaveInvoiceError e
    | AddDeposit ->
        { model with DepositModalIsOpenOn = Some Create }, Cmd.none
    | EditDeposit payment ->
        { model with DepositModalIsOpenOn = Some (Update payment) }, Cmd.none
    | RemoveDeposit toRemove ->
        model
        , showConfirmationModal 
            {| 
                Title = "Betaling verwijderen"
                Message = "Bent u er zeker van dat u de betaling wilt verwijderen?"
                OnConfirmed = (fun () -> ConfirmRemoveDeposit toRemove)
                OnDismissed = (fun () -> NoOp)
            |}
    | ConfirmRemoveDeposit payment ->
        model, Server.removeDeposit (model.CurrentBuilding.BuildingId, payment)
    | DepositWasCanceled ->
        { model with DepositModalIsOpenOn = None }, Cmd.none
    | DepositWasAdded createdOrUpdated ->
        match model.State with
        | State.Viewing detail ->
            let updatedDeposits =
                match createdOrUpdated with
                | DepositTypes.CreatedOrUpdated.Created deposit ->
                    deposit::detail.Deposits
                | DepositTypes.CreatedOrUpdated.Updated deposit ->
                    detail.Deposits |> List.map (fun p -> if p.DepositId = deposit.DepositId then deposit else p)
            model.NotifyEdited { detail with Deposits = updatedDeposits }
        | _ -> ()
        { model with State = Loading }, Server.getDepositRequestCmd model.DepositRequestId
    | DepositWasRemoved removed ->
        match model.State with
        | State.Viewing detail ->
            let updatedDeposits = detail.Deposits |> List.filter ((<>) removed)
            model.NotifyEdited { detail with Deposits = updatedDeposits }
        | _ -> ()
        model, Cmd.none
    | NoOp ->
        model, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    match model.State with
    | Loading ->  div [] [ str "Details worden geladen" ]
    | InvoiceNotFound -> div [] [ str "De door u gekozen provisie werd niet gevonden in de databank..." ]
    | Editing (isSaving, editState)
    | Creating (isSaving, editState) ->
        if isSaving then
            div [] [ str "De provisie wordt bewaard" ]
        else
            div [] [
                DepositRequestEditComponent.view editState (InvoiceEditMsg >> dispatch)

                div [ classes [ Bootstrap.card; Bootstrap.bgLight; Bootstrap.dInlineBlock ] ] [
                    div [ Class Bootstrap.cardBody ] [
                        button [ 
                            classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
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
            DepositRequestViewComponent.render {| DepositRequest = detail |}
            match detail.Deposits with
            | [] -> null
            | deposits ->
                fieldset [] [
                    yield! deposits |> List.sortBy (fun deposit -> deposit.Date) |> List.mapi (fun index deposit ->
                        DepositViewComponent.render
                            {|
                                Index = index
                                Deposit = deposit
                                OnEdit = EditDeposit >> dispatch
                                OnRemove = RemoveDeposit >> dispatch
                            |}
                    )
                ]
            div [ classes [ Bootstrap.card; Bootstrap.bgLight; Bootstrap.dInlineBlock ] ] [
                div [ Class Bootstrap.cardBody ] [
                    button [ 
                        classes [ Bootstrap.btn; Bootstrap.btnPrimary; Bootstrap.mr1 ]
                        OnClick (fun _ -> Edit detail |> dispatch) 
                    ] [
                        i [ classes [ FontAwesome.fa; FontAwesome.faEdit ] ] []
                        str " "
                        str "Provisie aanpassen"
                    ]
                    button [
                        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary; Bootstrap.mr1 ]
                        OnClick (fun _ -> dispatch AddDeposit)
                    ] [
                        i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                        str " "
                        str "Betaling toevoegen"
                    ]
                ]
            ]
            match model.DepositModalIsOpenOn with
            | Some createOrUpdate ->
                DepositModal.render
                    {|
                        DepositRequest = detail
                        CreateOrUpdate = createOrUpdate
                        CurrentBuilding = model.CurrentBuilding
                        FinancialCategories = model.FinancialCategories
                        OnSaved = fun createdOrUpdated -> dispatch (DepositWasAdded createdOrUpdated)
                        OnCanceled = fun () -> dispatch DepositWasCanceled
                    |}
            | None ->
                null
        ]



let render (props: DetailsProps) =
    React.elmishComponent ("DepositDetails", init props, update, view, string props.Identifier)