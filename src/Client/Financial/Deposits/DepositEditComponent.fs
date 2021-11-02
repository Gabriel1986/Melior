module Client.Financial.Deposits.DepositEditComponent

open System
open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React
open Shared.Read
open Shared.Library
open Shared.MediaLibrary
open Shared
open Shared.Trial
open Shared.Trial.Control
open Client.Upload
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Components
open Client.Components.BasicModal
open Client.Components.SelectionList
open DepositTypes

type DepositForm = 
    {
        LotOwner: FinancialLotOwner option
        DepositRequestId: Guid
        BuildingId: BuildingId
        DepositId: Guid
        Amount: string
        Date: DateTime
        FromBankAccount: BankAccount option
        FromFinancialCategory: FinancialCategory option
        ToFinancialCategory: FinancialCategory option
        MediaFiles: MediaFile list
    }
    static member Init (financialCategories: FinancialCategory list) (depositRequest: DepositRequest) =
        {
            DepositRequestId = depositRequest.DepositRequestId
            BuildingId = depositRequest.BuildingId
            DepositId = Guid.NewGuid()
            LotOwner = None
            Amount = ""
            Date = DateTime.Today
            FromBankAccount = None
            FromFinancialCategory = None
            ToFinancialCategory =
                //TODO: pull to server level
                match depositRequest.ToFinancialCategory.Code with
                | "700" -> financialCategories |> List.tryFind (fun cat -> cat.Code = "550")
                | "701"
                | "702" -> financialCategories |> List.tryFind (fun cat -> cat.Code = "551")
                | _ -> None
            MediaFiles = []
        }
    static member FromDeposit (deposit: Deposit) = {
        LotOwner = Some deposit.LotOwner
        DepositRequestId = deposit.DepositRequestId
        BuildingId = deposit.BuildingId
        DepositId = deposit.DepositId
        Amount = String.Format("{0:0.00}", deposit.Amount).Replace(".", ",")
        Date = deposit.Date
        FromBankAccount = deposit.FromBankAccount
        FromFinancialCategory = Some deposit.FromFinancialCategory
        ToFinancialCategory = Some deposit.ToFinancialCategory
        MediaFiles = deposit.MediaFiles
    }
    member me.Validate (): Result<Deposit, (string * string) list> =
        let validateAmount (path: string) (amount: string) =
            match Decimal.TryParse (amount.Replace(',', '.')) with
            | true, parsed -> Trial.Pass parsed
            | false, _ -> Trial.ofError (path, "De waarde die u heeft opgegeven is niet geldig")

        let validateMandatory (path: string) (opt: 'a option) =
            match opt with
            | Some filledIn -> Trial.Pass filledIn
            | None -> Trial.ofError (path, "Verplicht veld")

        trial {
            from toFinancialCategory in validateMandatory (nameof me.ToFinancialCategory) me.ToFinancialCategory
            also amount in validateAmount (nameof me.Amount) me.Amount
            also lotOwner in validateMandatory (nameof me.LotOwner) me.LotOwner
            also fromFinancialCategory in validateMandatory (nameof me.FromFinancialCategory) me.FromFinancialCategory
            yield {
                Deposit.DepositRequestId = me.DepositRequestId
                BuildingId = me.BuildingId
                DepositId = me.DepositId
                Amount = amount
                Date = me.Date
                FromBankAccount = me.FromBankAccount
                FromFinancialCategory = fromFinancialCategory
                ToFinancialCategory = toFinancialCategory
                MediaFiles = me.MediaFiles
                LotOwner = lotOwner
            }
        }
        |> Trial.toResult

type State = {
    CreateOrUpdate: CreateOrUpdate
    DepositRequest: DepositRequest
    Deposit: DepositForm
    CurrentBuilding: BuildingListItem
    Errors: (string * string) list
    LotOwners: FinancialLotOwner list
    LotOwnerModalIsOpen: bool
    FinancialCategories: FinancialCategory list
}

type Msg =
    | AmountChanged of string
    | DateChanged of DateTime
    | FromBankAccountChanged of BankAccount option
    | MediaFileAdded of MediaFile
    | MediaFileRemoved of MediaFile
    | LotOwnersLoaded of FinancialLotOwner list
    | ChangeLotOwner
    | LotOwnerChanged of FinancialLotOwner option
    | CancelChangeLotOwner
    | RemotingException of exn

type DepositEditComponentProps =
    {|
        DepositRequest: DepositRequest
        FinancialCategories: FinancialCategory list
        CurrentBuilding: BuildingListItem
        CreateOrUpdate: CreateOrUpdate
    |}

module Server =
    let getLotOwners (building: BuildingListItem) (request: DepositRequest) =
        Cmd.OfAsync.either 
            (Client.Remoting.getRemotingApi()).GetFinancialLotOwners 
                { 
                    BuildingId = building.BuildingId
                    Period = LotOwnerFilterPeriod.FinancialYear request.FinancialYear.FinancialYearId
                    DistributionKeyId = request.DistributionKey.DistributionKeyId
                }
                LotOwnersLoaded
                RemotingException

let init (props: DepositEditComponentProps) =
    {
        CreateOrUpdate = props.CreateOrUpdate
        DepositRequest = props.DepositRequest
        Deposit =
            match props.CreateOrUpdate with
            | Create -> DepositForm.Init props.FinancialCategories props.DepositRequest
            | Update deposit -> DepositForm.FromDeposit deposit
        CurrentBuilding = props.CurrentBuilding
        Errors = []
        LotOwners = []
        LotOwnerModalIsOpen = false
        FinancialCategories = props.FinancialCategories
    }, Server.getLotOwners (props.CurrentBuilding) (props.DepositRequest)

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let changePayment (changeFunc: DepositForm -> DepositForm) (state: State): State =
        { state with Deposit = changeFunc state.Deposit }

    match msg with
    | AmountChanged amount ->
        state |> changePayment (fun deposit -> { deposit with Amount = amount })
        , Cmd.none
    | DateChanged date ->
        state |> changePayment (fun deposit -> { deposit with Date = date })
        , Cmd.none
    | FromBankAccountChanged bankAccount ->
        state |> changePayment (fun deposit -> { deposit with FromBankAccount = bankAccount })
        , Cmd.none
    | MediaFileAdded newFile ->
        state |> changePayment (fun deposit -> { deposit with MediaFiles = newFile::deposit.MediaFiles })
        , Cmd.none
    | MediaFileRemoved removedFile ->
        state |> changePayment (fun deposit -> { deposit with MediaFiles = deposit.MediaFiles |> List.filter (fun file -> file.FileId <> removedFile.FileId) })
        , Cmd.none
    | LotOwnersLoaded lotOwners ->
        { state with LotOwners = lotOwners }
        , Cmd.none
    | ChangeLotOwner ->
        { state with LotOwnerModalIsOpen = true }
        , Cmd.none
    | LotOwnerChanged lotOwner ->
        { state with LotOwnerModalIsOpen = false }
        |> changePayment 
            (fun deposit -> {
                deposit with
                    LotOwner = lotOwner
                    FromFinancialCategory =
                        //TODO: pull to server level
                        match lotOwner, state.DepositRequest.ToFinancialCategory.Code with
                        | Some lotOwner, "700" -> state.FinancialCategories |> List.tryFind (fun funCat -> funCat.Code.StartsWith "4100" && funCat.LotOwnerId = Some lotOwner.LotOwnerId)
                        | Some lotOwner, "701" -> state.FinancialCategories |> List.tryFind (fun funCat -> funCat.Code.StartsWith "4101" && funCat.LotOwnerId = Some lotOwner.LotOwnerId)
                        | Some lotOwner, "702" -> state.FinancialCategories |> List.tryFind (fun funCat -> funCat.Code.StartsWith "400" && funCat.LotOwnerId = Some lotOwner.LotOwnerId)
                        | _ -> None
            })
        , Cmd.none
    | CancelChangeLotOwner ->
        { state with LotOwnerModalIsOpen = false }
        , Cmd.none
    | RemotingException error ->
        state, Client.Library.showGenericErrorModalCmd error

let private bankAccountToOption (state: State) (index: int) (account: BankAccount) = {
    Key = string index
    Label =
        [
            account.Description
            account.IBAN
        ]
        |> List.filter (String.IsNullOrEmpty >> not)
        |> String.joinWith " - "
    IsSelected = state.Deposit.FromBankAccount = Some account
}


let private renderFinancialLotOwnerSelectionModal (state: State) (dispatch: Msg -> unit) =
    BasicModal.render 
        {| 
            ModalProps = [
                IsOpen state.LotOwnerModalIsOpen
                DisableBackgroundClick false
                OnDismiss (fun _ -> dispatch CancelChangeLotOwner)
                Header [
                    HeaderProp.HasDismissButton true
                ]
                Body [
                    SelectionList.render
                        ({|
                            SelectionMode = SelectionMode.SingleSelect
                            AllItems = state.LotOwners
                            SelectedItems = state.Deposit.LotOwner |> Option.either List.singleton []
                            OnSelectionChanged = List.tryHead >> LotOwnerChanged >> dispatch
                            ListItemToString = fun financialLotOwner -> (sprintf "%s (%s)" financialLotOwner.LotOwnerType.Name financialLotOwner.LotCode)
                        |}, "FinancialLotOwnerSelectionList")
                ]
                Footer [
                    FooterProp.ShowDismissButton (Some "Annuleren")
                ]
            ]           
        |}

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
                        valueOrDefault (state.Deposit.Amount)
                    ]
                    FieldError (errorFor (nameof state.Deposit.Amount))
                ]
            ]
            div [ Class Bootstrap.colMd ] [
                formGroup [
                    Label "Datum"
                    Date [
                        Flatpickr.OnChange (fun e -> DateChanged e |> dispatch)
                        Flatpickr.Value state.Deposit.Date
                        Flatpickr.SelectionMode Flatpickr.Mode.Single
                        Flatpickr.EnableTimePicker false
                        Flatpickr.DateFormat "d/m/Y"
                    ]
                ]
            ]
            div [ Class Bootstrap.colMd ] [
                match state.Deposit.LotOwner with
                | None ->
                    div [] [ p [] [ str "Gelieve eerst aan te geven wie de storting heeft gedaan" ] ]
                | Some lotOwner ->
                    let bankAccounts =
                        match lotOwner.LotOwnerType with
                        | FinancialLotOwnerType.Owner owner -> owner.BankAccounts
                        | FinancialLotOwnerType.Organization org -> org.BankAccounts

                    let emptyOption: FormSelectOption = {
                        FormSelectOption.IsSelected = state.Deposit.FromBankAccount.IsNone
                        FormSelectOption.Key = "Other"
                        FormSelectOption.Label = "Andere"
                    }

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
                                |> (fun options -> emptyOption::options)
                        }
                    ]
            ]
        ]

        div [] [
            filePond 
                {|
                    BuildingId = Some state.CurrentBuilding.BuildingId
                    EntityId = state.Deposit.DepositId
                    Partition = Partitions.Deposits
                    Options = [
                        FilePondOptions.AllowMultiple true
                        FilePondOptions.MaxFiles 10
                        FilePondOptions.InitialFiles (state.Deposit.MediaFiles |> List.map (fun m -> m.FileId))
                        FilePondOptions.OnProcessFile (fun error filePondFile ->
                            if String.IsNullOrWhiteSpace(error) then
                                filePondFile 
                                |> FilePondFile.toMediaFile Partitions.Deposits (Some state.CurrentBuilding.BuildingId) state.Deposit.DepositId
                                |> MediaFileAdded
                                |> dispatch)
                        FilePondOptions.OnRemoveFile (fun error filePondFile ->
                            if String.IsNullOrWhiteSpace(error) then
                                filePondFile
                                |> FilePondFile.toMediaFile Partitions.InvoicePayments (Some state.CurrentBuilding.BuildingId) state.Deposit.DepositId
                                |> MediaFileRemoved
                                |> dispatch)
                    ]
                |}
        ]

        renderFinancialLotOwnerSelectionModal state dispatch
    ]
    |> fragment []