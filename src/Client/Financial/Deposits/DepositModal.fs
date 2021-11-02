module Client.Financial.Deposits.DepositModal

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Read
open Shared.Write
open Shared.Remoting
open Client.Library
open Client.Components
open Client.Components.BasicModal
open Client.Financial.Deposits.DepositEditComponent
open Client.ClientStyle
open Client.ClientStyle.Helpers
open DepositTypes

type DepositModalProps =
    {|
        DepositRequest: DepositRequest
        CreateOrUpdate: CreateOrUpdate
        CurrentBuilding: BuildingListItem
        FinancialCategories: FinancialCategory list
        OnSaved: CreatedOrUpdated -> unit
        OnCanceled: unit -> unit
    |}

type Model = {
    IsSaving: bool
    OnSaved: CreatedOrUpdated -> unit
    DepositEditComponentState: DepositEditComponent.State
    OnCanceled: unit -> unit
}
type Msg =
    | SaveModal
    | DepositSaved of Result<Deposit, SaveDepositError>
    | RemotingErrorOccured of exn
    | DepositEditComponentMsg of DepositEditComponent.Msg
    | CloseModal

let private init (props: DepositModalProps) =
    let componentState, componentCmd = 
        DepositEditComponent.init 
            {|
                DepositRequest = props.DepositRequest
                FinancialCategories = props.FinancialCategories
                CurrentBuilding = props.CurrentBuilding
                CreateOrUpdate = props.CreateOrUpdate
            |}

    {
        OnCanceled = props.OnCanceled
        OnSaved = props.OnSaved
        IsSaving = false
        DepositEditComponentState = componentState
    }, componentCmd |> Cmd.map DepositEditComponentMsg

module Server =
    let createDeposit (deposit: Deposit) =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).CreateDeposit
                deposit
                (Result.map (fun () -> deposit) >> DepositSaved)
                RemotingErrorOccured
    let updateDeposit (deposit: Deposit) =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).UpdateDeposit
                deposit
                (Result.map (fun () -> deposit) >> DepositSaved)
                RemotingErrorOccured

let private update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let setComponentErrors (errors: (string * string) list) =
        { model with DepositEditComponentState = { model.DepositEditComponentState with Errors = errors } }

    let processSaveDepositError =
        function
        | SaveDepositError.AuthorizationError ->
            model, showErrorToastCmd "U heeft geen toestemming om deze betaling te bewaren"
        | SaveDepositError.NotFound ->
            model, showErrorToastCmd "De betaling werd niet gevonden in de databank"
        | SaveDepositError.Validation errors ->
            setComponentErrors errors, Cmd.none

    match msg with
    | CloseModal ->
        model.OnCanceled ()
        model, Cmd.none
    | SaveModal ->
        let depositForm = model.DepositEditComponentState.Deposit
        match depositForm.Validate () with
        | Ok deposit ->
            match ValidatedDeposit.Validate deposit with
            | Ok _ ->
                match model.DepositEditComponentState.CreateOrUpdate with
                | Create   -> { model with IsSaving = true }, Server.createDeposit deposit
                | Update _ -> { model with IsSaving = true }, Server.updateDeposit deposit
            | Error errors ->
                setComponentErrors errors, Cmd.none
        | Error errors ->
            setComponentErrors errors, Cmd.none
    | DepositSaved result ->
        match result with
        | Ok deposit ->
            match model.DepositEditComponentState.CreateOrUpdate with
            | Create   -> model.OnSaved (Created deposit)
            | Update _ -> model.OnSaved (Updated deposit)
            model, Cmd.none
        | Error error ->
            processSaveDepositError error
    | RemotingErrorOccured e ->
        model, showGenericErrorModalCmd e
    | DepositEditComponentMsg msg ->
        let componentState, componentCmd = DepositEditComponent.update msg model.DepositEditComponentState
        { model with DepositEditComponentState = componentState }, componentCmd |> Cmd.map DepositEditComponentMsg

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
                    DepositEditComponent.view 
                        (model.DepositEditComponentState) 
                        (DepositEditComponentMsg >> dispatch)
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

let render (props: DepositModalProps) =
    React.elmishComponent ("DepositModal", init props, update, view)