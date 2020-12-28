module Client.Financial.Invoicing.InvoicePaymentEditComponent

open System
open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React
open Shared.Read
open Shared.Write
open Shared.Library
open Shared.MediaLibrary
open Client.Upload
open Client.Components
open Client.ClientStyle
open Client.ClientStyle.Helpers
open InvoicePaymentTypes

type State = {
    CreateOrUpdate: CreateOrUpdate
    Payment: InvoicePaymentInput
    CurrentBuilding: BuildingListItem
    FinancialCategories: FinancialCategory list
    ShowingFinancialCategoryModal: bool
    Errors: (string * string) list
}

type Msg =
    | AmountChanged of string
    | DateChanged of DateTime
    | FromBankAccountChanged of BankAccount option
    | ChangeCategory
    | CategoryChanged of FinancialCategory option
    | CancelChangeFinancialCategory
    | MediaFileAdded of MediaFile
    | MediaFileRemoved of MediaFile

type InvoicePaymentEditComponentProps =
    {|
        InvoiceId: Guid
        CurrentBuilding: BuildingListItem
        CreateOrUpdate: CreateOrUpdate
        FinancialCategories: FinancialCategory list
    |}

let init (props: InvoicePaymentEditComponentProps) =
    {
        CreateOrUpdate = props.CreateOrUpdate
        Payment =
            match props.CreateOrUpdate with
            | Create -> InvoicePaymentInput.Init props.CurrentBuilding props.InvoiceId
            | Update payment -> InvoicePaymentInput.FromInvoicePayment payment
        FinancialCategories = props.FinancialCategories
        CurrentBuilding = props.CurrentBuilding
        ShowingFinancialCategoryModal = false
        Errors = []
    }, Cmd.none

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let changePayment (changeFunc: InvoicePaymentInput -> InvoicePaymentInput): State =
        { state with Payment = changeFunc state.Payment }

    match msg with
    | AmountChanged amount ->
        changePayment (fun payment -> { payment with Amount = amount })
        , Cmd.none
    | DateChanged date ->
        changePayment (fun payment -> { payment with Date = date })
        , Cmd.none
    | FromBankAccountChanged bankAccount ->
        changePayment (fun payment -> { payment with FromBankAccount = bankAccount; FinancialCategoryId = bankAccount |> Option.bind (fun b -> b.FinancialCategoryId) })
        , Cmd.none
    | ChangeCategory ->
        { state with ShowingFinancialCategoryModal = true }
        , Cmd.none
    | CategoryChanged category ->
        changePayment (fun payment -> { payment with FinancialCategoryId = category |> Option.map (fun cat -> cat.FinancialCategoryId) })
        , Cmd.none
    | CancelChangeFinancialCategory ->
        { state with ShowingFinancialCategoryModal = false }
        , Cmd.none
    | MediaFileAdded newFile ->
        changePayment (fun payment -> { payment with MediaFiles = newFile::payment.MediaFiles })
        , Cmd.none
    | MediaFileRemoved removedFile ->
        changePayment (fun payment -> { payment with MediaFiles = payment.MediaFiles |> List.filter (fun file -> file.FileId <> removedFile.FileId) })
        , Cmd.none

let private bankAccountToOption (state: State) (index: int) (account: BankAccount) = {
    Key = string index
    Label =
        [
            account.Description
            account.IBAN
        ]
        |> List.filter (String.IsNullOrEmpty >> not)
        |> String.joinWith " - "
    IsSelected = state.Payment.FromBankAccount = Some account
}


let view (state: State) (dispatch: Msg -> unit) =
    let errorFor path = state.Errors |> List.tryPick (fun (ePath, error) -> if ePath = path then Some error else None)
    [
        div [ Class Bootstrap.row ] [
            div [ Class Bootstrap.colMd ] [
                formGroup [
                    Label "Bedrag"
                    InputPrepend [
                        span [ Class Bootstrap.inputGroupText ] [ str "€" ]
                    ]
                    Input [
                        Type "text"
                        OnChange (fun e -> AmountChanged e.Value |> dispatch)
                        valueOrDefault (state.Payment.Amount)
                    ]
                    FieldError (errorFor (nameof state.Payment.Amount))
                ]
            ]
            div [ Class Bootstrap.colMd ] [
                formGroup [
                    Label "Datum"
                    Date [
                        Flatpickr.OnChange (fun e -> DateChanged e |> dispatch)
                        Flatpickr.Value state.Payment.Date
                        Flatpickr.SelectionMode Flatpickr.Mode.Single
                        Flatpickr.EnableTimePicker false
                        Flatpickr.DateFormat "d/m/Y"
                    ]
                ]
            ]
        ]
        //MediaFiles: MediaFile list

        div [ Class Bootstrap.row ] [
            div [ Class Bootstrap.colMd ] [
                formGroup [
                    Label "Rekening"
                    Select {
                        Identifier = "FromBankAccount"
                        OnChanged = (fun key ->
                            state.CurrentBuilding.BankAccounts
                            |> List.tryItem (Int32.Parse key)
                            |> FromBankAccountChanged |> dispatch)
                        Options = state.CurrentBuilding.BankAccounts |> List.mapi (bankAccountToOption state)
                    }
                ]
            ]
            div [ Class Bootstrap.colMd ] [
                formGroup [
                    Label "Boekhoudkundige rekening"

                    let shown =
                        match state.Payment.FinancialCategoryId with
                        | Some categoryId ->
                            match state.FinancialCategories |> List.tryFind (fun cat -> cat.FinancialCategoryId = categoryId) with
                            | Some category -> sprintf "%s - %s" category.Code category.Description
                            | None -> ""
                        | _ -> ""
                    Input [
                        Type "text"
                        Style [ Cursor "pointer"; BackgroundColor "unset" ]
                        ReadOnly true
                        OnClick (fun _ -> dispatch ChangeCategory)
                        valueOrDefault shown
                    ]
                    InputAppend [
                        button [ 
                            classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]
                            OnClick (fun _ -> dispatch ChangeCategory)
                        ] [
                            span [ classes [ FontAwesome.fas; FontAwesome.faSearch ] ] []
                        ]
                    ]
                    FieldError (errorFor (nameof state.Payment.FinancialCategoryId))
                ]
            ]
        ]

        div [] [
            filePond 
                {|
                    BuildingId = Some state.CurrentBuilding.BuildingId
                    EntityId = state.Payment.InvoicePaymentId
                    Partition = Partitions.InvoicePayments
                    Options = [
                        FilePondOptions.AllowMultiple true
                        FilePondOptions.MaxFiles 10
                        FilePondOptions.InitialFiles (state.Payment.MediaFiles |> List.map (fun m -> m.FileId))
                        FilePondOptions.OnProcessFile (fun error filePondFile ->
                            if String.IsNullOrWhiteSpace(error) then
                                filePondFile 
                                |> FilePondFile.toMediaFile Partitions.InvoicePayments (Some state.CurrentBuilding.BuildingId) state.Payment.InvoicePaymentId
                                |> MediaFileAdded
                                |> dispatch)
                        FilePondOptions.OnRemoveFile (fun error filePondFile ->
                            if String.IsNullOrWhiteSpace(error) then
                                filePondFile
                                |> FilePondFile.toMediaFile Partitions.InvoicePayments (Some state.CurrentBuilding.BuildingId) state.Payment.InvoicePaymentId
                                |> MediaFileRemoved
                                |> dispatch)
                    ]
                |}
        ]

        FinancialCategorySelectionModal.render 
            {|
                SelectedCategoryId = state.Payment.FinancialCategoryId
                FinancialCategories = state.FinancialCategories
                Showing = state.ShowingFinancialCategoryModal
                OnCancelSelection = fun () -> dispatch (CancelChangeFinancialCategory)
                OnSelectionChanged = fun (selected) -> dispatch (CategoryChanged selected)
                Filter = Some (fun cat -> cat.Code.StartsWith("55"))
            |}
    ]
    |> fragment []