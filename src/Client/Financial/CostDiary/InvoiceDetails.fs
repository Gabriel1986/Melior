module Client.Financial.CostDiary.InvoiceDetails

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

type DetailsProps = {|
    CurrentUser: User
    CurrentBuilding: BuildingListItem
    Identifier: Guid
    IsNew: bool
    NotifyCreated: Invoice -> unit
    NotifyEdited: Invoice -> unit
|}

let private getInvoiceCmd invoiceId =
    Cmd.OfAsync.either
        (Remoting.getRemotingApi().GetInvoice)
        invoiceId
        View
        RemotingError

let init (props: DetailsProps): Model * Cmd<Msg> =
    let state, cmd =
        if props.IsNew then
            let invoiceEditState, invoiceEditCmd = InvoiceEditComponent.init (None, props.CurrentBuilding)
            Creating (false, invoiceEditState), invoiceEditCmd |> Cmd.map InvoiceEditMsg
        else
            Loading, getInvoiceCmd props.Identifier

    {
        CurrentUser = props.CurrentUser
        CurrentBuilding = props.CurrentBuilding
        InvoiceId = props.Identifier
        State = state
        NotifyCreated = props.NotifyCreated
        NotifyEdited = props.NotifyEdited
    }, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let processSaveInvoiceError =
        function
        | SaveInvoiceError.AuthorizationError ->
            model, showErrorToastCmd "U heeft geen toestemming om deze kavel te bewaren"
        | SaveInvoiceError.NotFound ->
            model, showErrorToastCmd "Het kavel werd niet gevonden in de databank"
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
        let invoiceEditState, invoiceEditCmd = InvoiceEditComponent.init (Some invoice, model.CurrentBuilding)
        { model with State = Editing (false, invoiceEditState) }, invoiceEditCmd |> Cmd.map InvoiceEditMsg
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


let view (model: Model) (dispatch: Msg -> unit) =
    match model.State with
    | Loading ->  div [] [ str "Details worden geladen" ]
    | InvoiceNotFound -> div [] [ str "Het door u gekozen kavel werd niet gevonden in de databank..." ]
    | Editing (isSaving, editState)
    | Creating (isSaving, editState) ->
        if isSaving 
        then
            div [] [ str "Het kavel wordt bewaard" ]
        else
            div [] [
                InvoiceEditComponent.view editState (InvoiceEditMsg >> dispatch)

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
            InvoiceViewComponent.render {| Invoice = detail |}
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
    React.elmishComponent ("InvoiceDetails", init props, update, view, string props.Identifier)

