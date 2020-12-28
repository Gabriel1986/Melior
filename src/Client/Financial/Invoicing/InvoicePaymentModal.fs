module Client.Financial.Invoicing.InvoicePaymentModal

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Read
open Shared.Write
open Shared.Trial
open Shared.Remoting
open Client.Library
open Client.Components
open Client.Components.BasicModal
open Client.Financial.Invoicing.InvoicePaymentEditComponent
open Client.ClientStyle
open Client.ClientStyle.Helpers
open InvoicePaymentTypes

type InvoicePaymentModalProps =
    {|
        InvoiceId: Guid
        CreateOrUpdate: CreateOrUpdate
        CurrentBuilding: BuildingListItem
        FinancialCategories: FinancialCategory list
        OnSaved: CreatedOrUpdated -> unit
        OnCanceled: unit -> unit
    |}

type Model = {
    IsSaving: bool
    OnSaved: CreatedOrUpdated -> unit
    InvoicePaymentEditComponentState: InvoicePaymentEditComponent.State
    OnCanceled: unit -> unit
}
type Msg =
    | SaveModal
    | InvoicePaymentSaved of Result<InvoicePaymentInput, SaveInvoicePaymentError>
    | RemotingErrorOccured of exn
    | InvoicePaymentEditComponentMsg of InvoicePaymentEditComponent.Msg
    | CloseModal

let private init (props: InvoicePaymentModalProps) =
    let componentState, componentCmd = 
        InvoicePaymentEditComponent.init 
            {|
                InvoiceId = props.InvoiceId
                CurrentBuilding = props.CurrentBuilding
                CreateOrUpdate = props.CreateOrUpdate
                FinancialCategories = props.FinancialCategories
            |}

    {
        OnCanceled = props.OnCanceled
        OnSaved = props.OnSaved
        IsSaving = false
        InvoicePaymentEditComponentState = componentState
    }, componentCmd |> Cmd.map InvoicePaymentEditComponentMsg

module Server =
    let createInvoicePayment (payment: InvoicePaymentInput) =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).CreateInvoicePayment
                payment
                (Result.map (fun () -> payment) >> InvoicePaymentSaved)
                RemotingErrorOccured
    let updateInvoicePayment (payment: InvoicePaymentInput) =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).UpdateInvoicePayment
                payment
                (Result.map (fun () -> payment) >> InvoicePaymentSaved)
                RemotingErrorOccured

let private update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let setComponentErrors (errors: (string * string) list) =
        { model with InvoicePaymentEditComponentState = { model.InvoicePaymentEditComponentState with Errors = errors } }

    let processSaveInvoiceError =
        function
        | SaveInvoicePaymentError.AuthorizationError ->
            model, showErrorToastCmd "U heeft geen toestemming om deze betaling te bewaren"
        | SaveInvoicePaymentError.NotFound ->
            model, showErrorToastCmd "De betaling werd niet gevonden in de databank"
        | SaveInvoicePaymentError.Validation errors ->
            setComponentErrors errors, Cmd.none

    match msg with
    | CloseModal ->
        model.OnCanceled ()
        model, Cmd.none
    | SaveModal ->
        let payment = model.InvoicePaymentEditComponentState.Payment
        match ValidatedInvoicePayment.Validate payment with
        | Ok validated ->
            match model.InvoicePaymentEditComponentState.CreateOrUpdate with
            | Create   -> { model with IsSaving = true }, Server.createInvoicePayment payment
            | Update _ -> { model with IsSaving = true }, Server.updateInvoicePayment payment
        | Error errors ->
            setComponentErrors errors, Cmd.none
    | InvoicePaymentSaved result ->
        match result with
        | Ok invoicePayment ->
            match model.InvoicePaymentEditComponentState.CreateOrUpdate with
            | Create   -> model.OnSaved (Created invoicePayment)
            | Update _ -> model.OnSaved (Updated invoicePayment)
            model, Cmd.none
        | Error error ->
            processSaveInvoiceError error
    | RemotingErrorOccured e ->
        model, showGenericErrorModalCmd e
    | InvoicePaymentEditComponentMsg msg ->
        let componentState, componentCmd = InvoicePaymentEditComponent.update msg model.InvoicePaymentEditComponentState
        { model with InvoicePaymentEditComponentState = componentState }, componentCmd |> Cmd.map InvoicePaymentEditComponentMsg

let private renderModalButtons (state: Model) (dispatch: Msg -> unit) = [
    button [ 
        classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
        OnClick (fun _ -> dispatch SaveModal) 
    ] [
        i [ classes [ FontAwesome.fa; FontAwesome.faSave ] ] []
        str " "
        str "Bewaren"
    ]
]

let private view (model: Model) (dispatch: Msg -> unit) =
    BasicModal.render
        {|
            ModalProps = [
                ModalProp.Header [
                    HeaderProp.Title "Betaling"
                    HeaderProp.HasDismissButton true
                ]
                ModalProp.Body [
                    InvoicePaymentEditComponent.view 
                        (model.InvoicePaymentEditComponentState) 
                        (InvoicePaymentEditComponentMsg >> dispatch)
                ]
                ModalProp.Footer [
                    FooterProp.ShowDismissButton (Some "Annuleren")
                    FooterProp.Buttons (renderModalButtons model dispatch)
                ]
                ModalProp.IsOpen true
                ModalProp.ModalSize ModalSize.LargeSize
                ModalProp.OnDismiss (fun () -> dispatch CloseModal)
            ]
        |}

let render (props: InvoicePaymentModalProps) =
    React.elmishComponent ("InvoicePaymentModal", init props, update, view)