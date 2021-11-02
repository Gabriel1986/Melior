module Client.Financial.Deposits.DepositRequestEditComponent

open System
open Client.Upload
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Shared.Read
open Shared.Library
open Shared.Trial
open Shared.Trial.Control
open Shared.MediaLibrary
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Library
open Client.Components
open Client.Components.BasicModal
open Client.Components.SelectionList
open Shared.ConstrainedTypes

type ReferenceType =
    | BelgianOGMReferenceType
type DepositRequestForm = 
    {
        DepositRequestId: Guid
        DepositRequestNumber: int option
        BuildingId: Guid
        FinancialYear: FinancialYear option
        Description: string option
        Amount: string option
        ToFinancialCategory: FinancialCategory option
        BookingDate: DateTimeOffset
        RequestDate: DateTimeOffset
        DueDate: DateTimeOffset
        //Reference: string option
        //ReferenceType: ReferenceType
        MediaFiles: MediaFile list
        Deposits: Deposit list
        ToBankAccount: BankAccount option
        DistributionKey: DistributionKeyListItem option
    }
    static member Init (buildingId: BuildingId): DepositRequestForm = {
        DepositRequestId = Guid.NewGuid()
        BuildingId = buildingId
        FinancialYear = None
        DepositRequestNumber = None
        Description = None
        Amount = None
        ToFinancialCategory = None
        BookingDate = DateTimeOffset.Now
        RequestDate = DateTimeOffset.Now
        DueDate = DateTimeOffset.Now.AddMonths(1)
        //Reference = None
        //ReferenceType = BelgianOGMReferenceType
        MediaFiles = []
        Deposits = []
        ToBankAccount = Some (BankAccount.Init ())
        DistributionKey = None
    } 
    static member fromDepositRequest (request: DepositRequest): DepositRequestForm =
        //let reference, referenceType =
        //    match request.Reference with
        //    | BelgianOGMReference ref -> ref, BelgianOGMReferenceType
        {
            DepositRequestId = request.DepositRequestId
            DepositRequestNumber = Some request.DepositRequestNumber
            BuildingId = request.BuildingId
            FinancialYear = Some request.FinancialYear
            Description = request.Description
            Amount = Some (String.Format("{0:0.00}", request.Amount).Replace(".", ","))
            ToFinancialCategory = Some request.ToFinancialCategory
            ToBankAccount = Some request.ToBankAccount
            BookingDate = request.BookingDate
            RequestDate = request.RequestDate
            DueDate = request.DueDate
            //Reference = Some reference
            //ReferenceType = referenceType
            MediaFiles = request.MediaFiles
            Deposits = request.Deposits
            DistributionKey = Some request.DistributionKey
        }

    member me.Validate (): Result<DepositRequest, (string * string) list> = 
        let mandatoryToTrial (path: string) (opt: 'T option) =
            match opt with
            | Some x -> Trial.Pass x
            | None -> Trial.ofError (path, "Verplicht veld")

        let validateAmount (cost) =
            match cost |> Option.defaultValue "" with
            | x when String.IsNullOrWhiteSpace(x) -> Trial.ofError (nameof me.Amount, "Verplicht veld")
            | x ->
                match Decimal.TryParse(x.Replace(',', '.')) with
                | true, parsed -> Trial.Pass parsed
                | false, _ -> Trial.ofError (nameof me.Amount, "Foutieve formattering")

        let validateReference (path: string) (referenceType: ReferenceType, reference: string option) =
            match referenceType, reference with
            | BelgianOGMReferenceType, Some reference ->
                Trial.Pass (BelgianOGMReference reference)
            | BelgianOGMReferenceType, None ->
                Trial.ofError (path, "Verplicht veld")

        trial {
            from financialYear in mandatoryToTrial (nameof me.FinancialYear) me.FinancialYear
            also toFinancialCategory in mandatoryToTrial (nameof me.ToFinancialCategory) me.ToFinancialCategory
            also distributionKey in mandatoryToTrial (nameof me.DistributionKey) me.DistributionKey
            also bankAccount in mandatoryToTrial (nameof me.ToBankAccount) me.ToBankAccount
            also amount in validateAmount me.Amount
            //also reference in validateReference (nameof me.Reference) (me.ReferenceType, me.Reference)
            yield {
                DepositRequest.DepositRequestId = me.DepositRequestId
                DepositRequestNumber = me.DepositRequestNumber  |> Option.defaultValue -1
                DistributionKey = distributionKey
                BuildingId = me.BuildingId
                FinancialYear = financialYear
                Description = me.Description
                Amount = amount
                ToFinancialCategory = toFinancialCategory
                RequestDate = me.RequestDate
                BookingDate = me.BookingDate
                MediaFiles = me.MediaFiles
                Deposits = me.Deposits
                //Reference = reference
                ToBankAccount = bankAccount
                DueDate = me.DueDate
            }
        }
        |> Trial.toResult

type Message =
    | AmountChanged of string
    | FinancialYearChanged of FinancialYear
    | FinancialYearsLoaded of FinancialYear list
    | FinancialCategoriesLoaded of FinancialCategory list
    | DistributionKeysLoaded of DistributionKeyListItem list
    | DescriptionChanged of string
    | BookingDateChanged of DateTimeOffset
    | RequestDateChanged of DateTimeOffset
    | DueDateChanged of DateTimeOffset
    | BankAccountChanged of BankAccount option

    | RemotingException of exn

    | ChangeDistributionKey
    | DistributionKeyChanged of DistributionKeyListItem option
    | CancelChangeDistributionKey

    | MediaFileAdded of MediaFile
    | MediaFileRemoved of MediaFile

module Server =
    let getFinancialYears (buildingId: BuildingId) =
        Cmd.OfAsync.either 
            (Client.Remoting.getRemotingApi().GetFinancialYears)
            buildingId
            FinancialYearsLoaded
            RemotingException
    let getFinancialCategories (buildingId: BuildingId) =
        Cmd.OfAsync.either 
            (Client.Remoting.getRemotingApi().GetFinancialCategories) 
            buildingId 
            FinancialCategoriesLoaded 
            RemotingException
    let getDistributionKeys (buildingId: BuildingId) =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi().GetDistributionKeyListItems)
            buildingId
            DistributionKeysLoaded
            RemotingException

type State = {
    Request: DepositRequestForm
    FinancialYears: FinancialYear list
    LoadingFinancialYears: bool
    FinancialCategories: FinancialCategory list
    LoadingFinancialCategories: bool
    DistributionKeys: DistributionKeyListItem list
    LoadingDistributionKeys: bool
    ShowingDistributionKeyModal: bool
    CurrentBuilding: BuildingListItem
    Errors: (string * string) list
}

type InvoiceEditComponentProps =
    {|
        DepositRequest: DepositRequest option
        CurrentBuilding: BuildingListItem
    |}

let init (props: InvoiceEditComponentProps) =
    let inputModel =
        match props.DepositRequest with
        | Some request ->
            DepositRequestForm.fromDepositRequest (request)
        | None ->
            DepositRequestForm.Init (props.CurrentBuilding.BuildingId)

    {
        Request = inputModel
        FinancialYears = []
        LoadingFinancialYears = true
        FinancialCategories = []
        LoadingFinancialCategories = true
        DistributionKeys = []
        LoadingDistributionKeys = true
        ShowingDistributionKeyModal = false
        Errors = []
        CurrentBuilding = props.CurrentBuilding
    }
    , Cmd.batch [
        Server.getFinancialCategories inputModel.BuildingId
        Server.getFinancialYears inputModel.BuildingId
        Server.getDistributionKeys inputModel.BuildingId
    ]

let update (message: Message) (state: State): State * Cmd<Message> =
    printf "Errors: %A" state.Errors
    let changeDepositRequest f state =
        { state with Request = f state.Request }

    let recalculateValidationErrors (state: State) =
        match state.Errors with
        | [] -> state
        | _errors ->
            match state.Request.Validate() with
            | Ok _validated -> state
            | Error validationErrors -> { state with Errors = validationErrors }

    match message with
    | AmountChanged x ->
        let newAmount = if String.IsNullOrEmpty(x) then None else Some x
        state |> changeDepositRequest (fun request -> { request with Amount = newAmount })
        , Cmd.none
    | FinancialYearChanged financialYear ->
        if state.Request.FinancialYear <> Some financialYear then
            state |> changeDepositRequest (fun request -> { request with FinancialYear = Some financialYear })
            , Cmd.none
        else
            state, Cmd.none
    | DescriptionChanged description ->
        let newDescription = if String.IsNullOrEmpty(description) then None else Some description
        state |> changeDepositRequest (fun request -> { request with Description = newDescription })
        , Cmd.none
    | BookingDateChanged bookingDate ->
        state |> changeDepositRequest (fun request -> { request with BookingDate = bookingDate })
        , Cmd.none
    | RequestDateChanged requestDate ->
        state |> changeDepositRequest (fun request -> { request with RequestDate = requestDate })
        , Cmd.none
    | DueDateChanged dueDate ->
        state |> changeDepositRequest (fun request -> { request with DueDate = dueDate })
        , Cmd.none
    | BankAccountChanged account ->
        state |> changeDepositRequest (fun request -> { 
            request with 
                ToBankAccount = account
                ToFinancialCategory =
                    match account with
                    | x when x = state.CurrentBuilding.SavingsBankAccount ->
                        state.FinancialCategories |> List.tryFind (fun fc -> fc.Code = "700")
                    | x when x = state.CurrentBuilding.CheckingBankAccount ->
                        state.FinancialCategories |> List.tryFind (fun fc -> fc.Code = "701")
                    | _ ->
                        None
        })
        , Cmd.none
    | FinancialYearsLoaded financialYears ->
        let newState =
            { state with LoadingFinancialYears = false; FinancialYears = financialYears }
            |> changeDepositRequest (fun request -> 
                match request.FinancialYear with 
                | None -> { request with FinancialYear = financialYears |> List.tryHead } 
                | Some _ -> request)

        newState, Cmd.none
    | FinancialCategoriesLoaded financialCategories ->
        { state with LoadingFinancialCategories = false; FinancialCategories = financialCategories }
        , Cmd.none
    | DistributionKeysLoaded distributionKeys ->
        { state with LoadingDistributionKeys = false; DistributionKeys = distributionKeys }
        , Cmd.none
    | RemotingException error ->
        state, showGenericErrorModalCmd error
    | MediaFileAdded mediaFile ->
        state |> changeDepositRequest (fun i -> { i with MediaFiles = mediaFile::i.MediaFiles })
        , Cmd.none
    | MediaFileRemoved mediaFile ->
        state |> changeDepositRequest (fun i -> { i with MediaFiles = i.MediaFiles |> List.filter (fun m -> m.FileId = mediaFile.FileId) })
        , Cmd.none

    | ChangeDistributionKey ->
        { state with ShowingDistributionKeyModal = true }, Cmd.none
    | DistributionKeyChanged distributionKey ->
        { state with ShowingDistributionKeyModal = false }
        |> changeDepositRequest (fun i -> { i with DistributionKey = distributionKey })
        , Cmd.none
    | CancelChangeDistributionKey ->
        { state with ShowingDistributionKeyModal = false }, Cmd.none

    |> (fun (state, cmd) -> state |> recalculateValidationErrors, cmd)

let inColumn x = div [ Class Bootstrap.col ] [ x ]
let inColumn' colClass x = div [ Class colClass ] [ x ]

let private renderDistributionKeyModal (props: {| SelectedDistributionKey: DistributionKeyListItem option; AllDistributionKeys: DistributionKeyListItem list; Showing: bool; Dispatch: Message -> unit |}) =
    BasicModal.render
        {| 
            ModalProps = [
                IsOpen props.Showing
                DisableBackgroundClick false
                OnDismiss (fun _ -> props.Dispatch CancelChangeDistributionKey)
                Header [
                    HeaderProp.Title "Verdeelsleutel selecteren"
                    HeaderProp.HasDismissButton true
                ]
                Body [
                    SelectionList.render (
                        {|
                            SelectionMode = SelectionMode.SingleSelect
                            AllItems = props.AllDistributionKeys |> List.sortBy (fun cat -> cat.Name)
                            SelectedItems = [ props.SelectedDistributionKey ] |> List.choose id
                            OnSelectionChanged = fun selection -> DistributionKeyChanged (selection |> List.tryHead) |> props.Dispatch
                            ListItemToString = fun dKey -> dKey.Name
                        |}, "DistributionKeySelectionList")
                ]
                Footer [
                    //yield FooterProp.Buttons []
                ]
            ]
        |}

let view (state: State) (dispatch: Message -> unit) =
    let errorFor (path: string) =
        state.Errors |> List.tryPick (fun (p, error) -> if p = path then Some error else None)

    let bankAccountErrorFor (path: string) =
        errorFor (sprintf "%s.%s" (nameof state.Request.ToBankAccount) path)

    let financialYearToOption (financialYear: FinancialYear): FormSelectOption = {
        Key = sprintf "%A" financialYear.FinancialYearId
        Label = financialYear.Code
        IsSelected = state.Request.FinancialYear = Some financialYear
    }

    let financialYearSelection: FormSelect = {
        Identifier = "financialYear"
        OnChanged = (fun e -> 
            FinancialYearChanged (state.FinancialYears |> List.find (fun year -> string year.FinancialYearId = e)) 
            |> dispatch)
        Options = state.FinancialYears |> List.map financialYearToOption
    }

    let bankAccountSelection: FormSelect = 
        let possibleBankAccounts = 
            [ state.CurrentBuilding.SavingsBankAccount; state.CurrentBuilding.CheckingBankAccount ]
            |> List.choose id

        let bankAccountToOption (index: int) (account: BankAccount): FormSelectOption = {
            Key = string index
            Label =
                [ 
                    account.Description
                    account.IBAN 
                ] 
                |> List.filter (String.IsNullOrEmpty >> not) 
                |> String.joinWith " - "
            IsSelected = state.Request.ToBankAccount = Some account
        }

        {
            Identifier = "toAccount"
            OnChanged = (fun e -> BankAccountChanged (possibleBankAccounts |> List.indexed |> List.tryPick (fun (index, bankAccount) -> if (string index) = e then Some bankAccount else None)) |> dispatch)
            Options = possibleBankAccounts |> List.mapi bankAccountToOption
        }

    [
        div [ Class Bootstrap.row ] [
            formGroup [ 
                Label "Boekjaar"
                Select financialYearSelection
                FieldError (errorFor (nameof state.Request.FinancialYear))
            ]
            |> inColumn

            formGroup [
                Label "Boekingsdatum"
                Date [
                    Flatpickr.OnChange (fun e -> BookingDateChanged (new DateTimeOffset(e)) |> dispatch)
                    Flatpickr.Value (state.Request.BookingDate.LocalDateTime)
                    Flatpickr.SelectionMode Flatpickr.Mode.Single
                    Flatpickr.EnableTimePicker false
                    Flatpickr.DateFormat "d/m/Y"
                ]
                FieldError (errorFor (nameof state.Request.BookingDate))
            ]
            |> inColumn

            formGroup [
                Label "Verdeelsleutel"
                Input [
                    Type "text"
                    ReadOnly true
                    Style [ BackgroundColor "unset"; Cursor "pointer" ]
                    OnClick (fun _ -> dispatch ChangeDistributionKey)
                    valueOrDefault (state.Request.DistributionKey |> Option.map (fun o -> o.Name) |> Option.defaultValue "")
                ]
                InputAppend [
                    button [ 
                        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]
                        OnClick (fun _ -> ChangeDistributionKey |> dispatch)
                    ] [
                        span [ classes [ FontAwesome.fas; FontAwesome.faSearch ] ] []
                    ]
                ]
                FieldError (errorFor (nameof state.Request.DistributionKey))
            ]
            |> inColumn
        ]
        div [ Class Bootstrap.row ] [
            formGroup [
                Label "Voorschotbedrag (Incl. BTW)"
                InputPrepend [
                    span [ Class Bootstrap.inputGroupText ] [ str "€" ]
                ]
                Input [
                    Type "text"
                    OnChange (fun e -> AmountChanged e.Value |> dispatch)
                    valueOrDefault (state.Request.Amount |> Option.defaultValue "")
                ]
                FieldError (errorFor (nameof state.Request.Amount))
            ]
            |> inColumn

            formGroup [
                Label "Aanvraagdatum"
                Date [
                    Flatpickr.OnChange (fun e -> RequestDateChanged (new DateTimeOffset(e)) |> dispatch)
                    Flatpickr.Value (state.Request.RequestDate.LocalDateTime)
                    Flatpickr.SelectionMode Flatpickr.Mode.Single
                    Flatpickr.EnableTimePicker false
                    Flatpickr.DateFormat "d/m/Y"
                ]
                FieldError (errorFor (nameof state.Request.RequestDate))
            ]
            |> inColumn

            formGroup [
                Label "Uiterste betaaldatum"
                Date [
                    Flatpickr.OnChange (fun e -> DueDateChanged (new DateTimeOffset(e)) |> dispatch)
                    Flatpickr.Value (state.Request.DueDate.LocalDateTime)
                    Flatpickr.SelectionMode Flatpickr.Mode.Single
                    Flatpickr.EnableTimePicker false
                    Flatpickr.DateFormat "d/m/Y"
                ]
                FieldError (errorFor (nameof state.Request.DueDate))
            ]
            |> inColumn
        ]

        div [ Class Bootstrap.row ] [
            formGroup [
                Label "Naar rekening"
                Select bankAccountSelection
                FieldError (errorFor (nameof state.Request.ToBankAccount))
            ]
            |> inColumn' Bootstrap.col5

            formGroup [
                Label "IBAN"
                Input [
                    Type "text"
                    MaxLength 64.0
                    Disabled true
                    valueOrDefault (state.Request.ToBankAccount |> Option.map (fun account -> account.IBAN) |> Option.defaultValue "")
                ]
                FieldError (bankAccountErrorFor (nameof state.Request.ToBankAccount.Value.IBAN))
            ]
            |> inColumn
            formGroup [
                Label "BIC"
                Input [
                    Type "text"
                    MaxLength 11.0
                    Disabled true
                    valueOrDefault (state.Request.ToBankAccount |> Option.map (fun account -> account.BIC) |> Option.defaultValue "")
                ]
                FieldError (bankAccountErrorFor (nameof state.Request.ToBankAccount.Value.BIC))
            ]
            |> inColumn
        ]

        div [] [
            filePond 
                {|
                    BuildingId = Some state.CurrentBuilding.BuildingId
                    EntityId = state.Request.DepositRequestId
                    Partition = Partitions.DepositRequests
                    Options = [
                        FilePondOptions.AllowMultiple true
                        FilePondOptions.MaxFiles 10
                        FilePondOptions.InitialFiles (state.Request.MediaFiles |> List.map (fun m -> m.FileId))
                        FilePondOptions.OnProcessFile (fun error filePondFile ->
                            if String.IsNullOrWhiteSpace(error) then
                                filePondFile 
                                |> FilePondFile.toMediaFile Partitions.DepositRequests (Some state.CurrentBuilding.BuildingId) state.Request.DepositRequestId
                                |> MediaFileAdded
                                |> dispatch)
                        FilePondOptions.OnRemoveFile (fun error filePondFile ->
                            if String.IsNullOrWhiteSpace(error) then
                                filePondFile
                                |> FilePondFile.toMediaFile Partitions.Invoices (Some state.CurrentBuilding.BuildingId) state.Request.DepositRequestId
                                |> MediaFileRemoved
                                |> dispatch)
                    ]
                |}
        ]

        renderDistributionKeyModal
            {|
                Showing = state.ShowingDistributionKeyModal
                SelectedDistributionKey = state.Request.DistributionKey
                AllDistributionKeys = state.DistributionKeys
                Dispatch = dispatch
            |}
    ]
    |> fragment []