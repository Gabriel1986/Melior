module Client.Financial.Invoicing.InvoiceDetails

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
    InvoiceId: Guid
    CurrentBuilding: BuildingListItem
    CurrentUser: User
    State: State
    NotifyCreated: Invoice -> unit
    NotifyEdited:  Invoice -> unit
    NotifyPaymentAdded: Invoice * InvoicePaymentInput -> unit
    NotifyPaymentUpdated: Invoice * InvoicePaymentInput -> unit
    PaymentModalIsOpenOn: InvoicePaymentTypes.CreateOrUpdate option
    LoadingFinancialCategories: bool
    FinancialCategories: FinancialCategory list
}
and State =
    | Loading
    | Viewing  of detail: Invoice
    | Editing  of isSaving: bool * invoiceEditState: InvoiceEditComponent.State
    | Creating of isSaving: bool * invoiceEditState: InvoiceEditComponent.State
    | InvoiceNotFound

type Msg =
    | InvoiceEditMsg of InvoiceEditComponent.Message
    | View of Invoice option
    | Edit of Invoice
    | RemotingError of exn
    | Save
    | ProcessCreateResult of Result<Invoice, SaveInvoiceError>
    | ProcessUpdateResult of Result<Invoice, SaveInvoiceError>

    | AddPayment
    | EditPayment of InvoicePayment
    | RemovePayment of InvoicePayment
    | ConfirmRemovePayment of InvoicePayment
    | PaymentWasCanceled
    | PaymentWasAdded of InvoicePaymentTypes.CreatedOrUpdated
    | PaymentWasRemoved of InvoicePayment
    | FinancialCategoriesLoaded of FinancialCategory list
    | NoOp

type DetailsProps = {|
    CurrentUser: User
    CurrentBuilding: BuildingListItem
    Identifier: Guid
    IsNew: bool
    NotifyCreated: Invoice -> unit
    NotifyEdited: Invoice -> unit
    NotifyPaymentAdded: Invoice * InvoicePaymentInput -> unit
    NotifyPaymentUpdated: Invoice * InvoicePaymentInput -> unit
|}

module Server =
    let getInvoiceCmd invoiceId =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetInvoice)
            invoiceId
            View
            RemotingError

    let removePayment (buildingId: BuildingId, invoicePayment: InvoicePayment) =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().DeleteInvoicePayment)
            (buildingId, invoicePayment.InvoicePaymentId)
            (fun _ -> PaymentWasRemoved invoicePayment)
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
                InvoiceEditComponent.init {| CurrentBuilding = props.CurrentBuilding; Invoice = None |}
            Creating (false, invoiceEditState), invoiceEditCmd |> Cmd.map InvoiceEditMsg
        else
            Loading, Server.getInvoiceCmd props.Identifier

    {
        CurrentUser = props.CurrentUser
        CurrentBuilding = props.CurrentBuilding
        InvoiceId = props.Identifier
        State = state
        NotifyCreated = props.NotifyCreated
        NotifyEdited = props.NotifyEdited
        NotifyPaymentAdded = props.NotifyPaymentAdded
        NotifyPaymentUpdated = props.NotifyPaymentUpdated
        PaymentModalIsOpenOn = None
        FinancialCategories = []
        LoadingFinancialCategories = true
    }, Cmd.batch [ cmd; Server.getFinancialCategories props.CurrentBuilding.BuildingId ]

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let processSaveInvoiceError =
        function
        | SaveInvoiceError.AuthorizationError ->
            model, showErrorToastCmd "U heeft geen toestemming om deze factuur te bewaren"
        | SaveInvoiceError.NotFound ->
            model, showErrorToastCmd "De factuur werd niet gevonden in de databank"
        | SaveInvoiceError.Validation errors ->
            match model.State with
            | Creating (_, componentState) ->
                { model with State = Creating (false, { componentState with Errors = errors }) }, Cmd.none
            | Editing (_, componentState) ->
                { model with State = Editing (false, { componentState with Errors = errors }) }, Cmd.none
            | _ ->
                model, Cmd.none

    match msg with
    | View invoice ->
        match invoice with
        | Some invoice ->
            { model with State = Viewing invoice }, Cmd.none
        | None ->
            { model with State = InvoiceNotFound }, Cmd.none
    | Edit invoice ->
        let invoiceEditState, invoiceEditCmd = InvoiceEditComponent.init {| Invoice = Some invoice; CurrentBuilding = model.CurrentBuilding |}
        { model with State = Editing (false, invoiceEditState) }, invoiceEditCmd |> Cmd.map InvoiceEditMsg
    | FinancialCategoriesLoaded categories ->
        { model with FinancialCategories = categories; LoadingFinancialCategories = false }, Cmd.none
    | InvoiceEditMsg componentMsg ->
        let updateComponentState s (isSaving, componentState) =
            let newComponentState, newComponentCmd = InvoiceEditComponent.update componentMsg componentState
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
            match componentState.Invoice.Validate () with
            | Ok invoice ->
                match ValidatedInvoice.Validate invoice with
                | Ok _ ->
                    let cmd = 
                        Cmd.OfAsync.either
                            (Remoting.getRemotingApi().UpdateInvoice)
                            invoice
                            (fun result -> result |> Result.map (fun _ -> invoice) |> ProcessUpdateResult)
                            RemotingError
                    { model with State = Editing (true, componentState) }, cmd
                | Error e ->
                    { model with State = Editing (false, { componentState with Errors = e }) }, Cmd.none
            | Error e ->
                { model with State = Editing (false, { componentState with Errors = e }) }, Cmd.none
        | Creating (_, componentState) ->
            match componentState.Invoice.Validate () with
            | Ok invoice ->
                match ValidatedInvoice.Validate invoice with
                | Ok _ ->
                    let cmd = 
                        Cmd.OfAsync.either
                            (Remoting.getRemotingApi().CreateInvoice)
                            invoice
                            (fun result -> result |> Result.map (fun _ -> invoice) |> ProcessCreateResult)
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
    | AddPayment ->
        { model with PaymentModalIsOpenOn = Some (InvoicePaymentTypes.CreateOrUpdate.Create) }, Cmd.none
    | EditPayment payment ->
        { model with PaymentModalIsOpenOn = Some (InvoicePaymentTypes.CreateOrUpdate.Update payment) }, Cmd.none
    | RemovePayment toRemove ->
        model
        , showConfirmationModal 
            {| 
                Title = "Betaling verwijderen"
                Message = "Bent u er zeker van dat u de betaling wilt verwijderen?"
                OnConfirmed = (fun () -> ConfirmRemovePayment toRemove)
                OnDismissed = (fun () -> NoOp)
            |}
    | ConfirmRemovePayment payment ->
        model, Server.removePayment (model.CurrentBuilding.BuildingId, payment)
    | PaymentWasCanceled ->
        { model with PaymentModalIsOpenOn = None }, Cmd.none
    | PaymentWasAdded createdOrUpdated ->
        match model.State with
        | State.Viewing detail ->
            match createdOrUpdated with
            | InvoicePaymentTypes.CreatedOrUpdated.Created payment ->
                model.NotifyPaymentAdded (detail, payment)
            | InvoicePaymentTypes.CreatedOrUpdated.Updated payment ->
                model.NotifyPaymentUpdated (detail, payment)
        | _ -> ()
        { model with State = Loading }, Server.getInvoiceCmd model.InvoiceId
    | PaymentWasRemoved removed ->
        match model.State with
        | State.Viewing detail ->
            let updatedPayments = detail.Payments |> List.filter ((<>) removed)
            model.NotifyEdited { detail with Payments = updatedPayments }
        | _ -> ()
        model, Cmd.none
    | NoOp ->
        model, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    match model.State with
    | Loading ->  div [] [ str "Details worden geladen" ]
    | InvoiceNotFound -> div [] [ str "De door u gekozen factuur werd niet gevonden in de databank..." ]
    | Editing (isSaving, editState)
    | Creating (isSaving, editState) ->
        if isSaving then
            div [] [ str "De factuur wordt bewaard" ]
        elif model.LoadingFinancialCategories then
            div [] [ str "Details worden geladen" ]
        else
            div [] [
                InvoiceEditComponent.view model.FinancialCategories editState (InvoiceEditMsg >> dispatch)

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
            InvoiceViewComponent.render {| Invoice = detail |}
            match detail.Payments with
            | [] -> null
            | payments ->
                fieldset [] [
                    if model.LoadingFinancialCategories then
                        yield div [] [ str "Details worden geladen..." ]
                    else
                        yield! payments |> List.sortBy (fun payment -> payment.Date) |> List.mapi (fun index payment ->
                            InvoicePaymentViewComponent.render
                                {|
                                    Index = index
                                    InvoicePayment = payment
                                    FinancialCategory = Some payment.FinancialCategory
                                    OnEdit = EditPayment >> dispatch
                                    OnRemove = RemovePayment >> dispatch
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
                        str "Factuur aanpassen"
                    ]
                    button [
                        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary; Bootstrap.mr1 ]
                        OnClick (fun _ -> dispatch AddPayment)
                    ] [
                        i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                        str " "
                        str "Betaling toevoegen"
                    ]
                ]
            ]
            match model.PaymentModalIsOpenOn with
            | Some createOrUpdate ->
                InvoicePaymentModal.render
                    {|
                        InvoiceId = detail.InvoiceId
                        CreateOrUpdate = createOrUpdate
                        CurrentBuilding = model.CurrentBuilding
                        FinancialCategories = model.FinancialCategories
                        OnSaved = fun createdOrUpdated -> dispatch (PaymentWasAdded createdOrUpdated)
                        OnCanceled = fun () -> dispatch PaymentWasCanceled
                    |}
            | None ->
                null
        ]



let render (props: DetailsProps) =
    React.elmishComponent ("InvoiceDetails", init props, update, view, string props.Identifier)

