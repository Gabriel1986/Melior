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
open Shared
open Shared.Trial
open Shared.Trial.Control
open Client.Upload
open Client.Components
open Client.ClientStyle
open Client.ClientStyle.Helpers
open InvoicePaymentTypes

type InvoicePaymentForm = 
    {
        InvoiceId: Guid
        BuildingId: BuildingId
        InvoicePaymentId: Guid
        Amount: string
        Date: DateTime
        FromBankAccount: BankAccount option
        FinancialCategory: FinancialCategory option
        MediaFiles: MediaFile list
    }
    static member Init (financialCategories: FinancialCategory list) (currentBuilding: BuildingListItem) (invoiceId: Guid) =
        let checkingAccount = currentBuilding.CheckingBankAccount
        let savingsAccount = currentBuilding.SavingsBankAccount
        {
            InvoiceId = invoiceId
            BuildingId = currentBuilding.BuildingId
            InvoicePaymentId = Guid.NewGuid()
            Amount = ""
            Date = DateTime.Today
            FromBankAccount = checkingAccount |> Option.orElse savingsAccount
            FinancialCategory =
                match checkingAccount, savingsAccount with
                | Some _, _ -> financialCategories |> List.tryFind (fun cat -> cat.Code = "551")
                | None, Some _ -> financialCategories |> List.tryFind (fun cat -> cat.Code = "550")
                | _ -> None
            MediaFiles = []
        }
    static member FromInvoicePayment (payment: InvoicePayment) = {
        InvoiceId = payment.InvoiceId
        BuildingId = payment.BuildingId
        InvoicePaymentId = payment.InvoicePaymentId
        Amount = String.Format("{0:0.00}", payment.Amount).Replace(".", ",")
        Date = payment.Date
        FromBankAccount = Some payment.FromBankAccount
        FinancialCategory = Some payment.FinancialCategory
        MediaFiles = payment.MediaFiles
    }
    member me.Validate (): Result<InvoicePayment, (string * string) list> =
        let validateAmount (path: string) (amount: string) =
            match Decimal.TryParse (amount.Replace(',', '.')) with
            | true, parsed -> Trial.Pass parsed
            | false, _ -> Trial.ofError (path, "De waarde die u heeft opgegeven is niet geldig")

        let validateMandatory (path: string) (opt: 'a option) =
            match opt with
            | Some filledIn -> Trial.Pass filledIn
            | None -> Trial.ofError (path, "Verplicht veld")

        trial {
            from bankAccount in validateMandatory (nameof me.FromBankAccount) me.FromBankAccount
            also amount in validateAmount (nameof me.Amount) me.Amount
            also financialCategory in validateMandatory (nameof me.FinancialCategory) me.FinancialCategory
            yield {
                InvoicePayment.InvoiceId = me.InvoiceId
                BuildingId = me.BuildingId
                InvoicePaymentId = me.InvoicePaymentId
                Amount = amount
                Date = me.Date
                FromBankAccount = bankAccount
                FinancialCategory = financialCategory
                MediaFiles = me.MediaFiles
            }
        }
        |> Trial.toResult

type State = {
    CreateOrUpdate: CreateOrUpdate
    Payment: InvoicePaymentForm
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
            | Create -> InvoicePaymentForm.Init props.FinancialCategories props.CurrentBuilding props.InvoiceId
            | Update payment -> InvoicePaymentForm.FromInvoicePayment payment
        FinancialCategories = props.FinancialCategories
        CurrentBuilding = props.CurrentBuilding
        ShowingFinancialCategoryModal = false
        Errors = []
    }, Cmd.none

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let changePayment (changeFunc: InvoicePaymentForm -> InvoicePaymentForm): State =
        { state with Payment = changeFunc state.Payment }

    match msg with
    | AmountChanged amount ->
        changePayment (fun payment -> { payment with Amount = amount })
        , Cmd.none
    | DateChanged date ->
        changePayment (fun payment -> { payment with Date = date })
        , Cmd.none
    | FromBankAccountChanged bankAccount ->
        changePayment (fun payment -> { 
            payment with 
                FromBankAccount = bankAccount
                FinancialCategory =
                    match bankAccount with
                    | Some acc when Some acc = state.CurrentBuilding.CheckingBankAccount ->
                        state.FinancialCategories |> List.tryFind (fun cat -> cat.Code = "551")
                    | Some acc when Some acc = state.CurrentBuilding.SavingsBankAccount ->
                        state.FinancialCategories |> List.tryFind (fun cat -> cat.Code = "550")
                    | _ ->
                        None
        })
        , Cmd.none
    | ChangeCategory ->
        { state with ShowingFinancialCategoryModal = true }
        , Cmd.none
    | CategoryChanged category ->
        changePayment (fun payment -> { payment with FinancialCategory = category })
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
                let bankAccounts =
                    [ state.CurrentBuilding.CheckingBankAccount; state.CurrentBuilding.SavingsBankAccount ] 
                    |> List.choose id

                formGroup [
                    Label "Rekening"
                    Select {
                        Identifier = "FromBankAccount"
                        OnChanged = (fun key ->
                            bankAccounts
                            |> List.tryItem (Int32.Parse key)
                            |> FromBankAccountChanged |> dispatch)
                        Options =
                            bankAccounts
                            |> List.mapi (bankAccountToOption state)
                    }
                ]
            ]
            div [ Class Bootstrap.colMd ] [
                formGroup [
                    Label "Boekhoudkundige rekening"

                    let shown =                        
                        match state.Payment.FinancialCategory with
                        | Some category -> sprintf "%s - %s" category.Code category.Description
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
                    FieldError (errorFor (nameof state.Payment.FinancialCategory))
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
                SelectedCategoryId = state.Payment.FinancialCategory |> Option.map (fun cat -> cat.FinancialCategoryId)
                FinancialCategories = state.FinancialCategories
                Showing = state.ShowingFinancialCategoryModal
                OnCancelSelection = fun () -> dispatch (CancelChangeFinancialCategory)
                OnSelectionChanged = fun (selected) -> dispatch (CategoryChanged selected)
                Filter = Some (fun cat -> cat.Code.StartsWith("55"))
            |}
    ]
    |> fragment []