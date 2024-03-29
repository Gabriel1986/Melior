﻿module Client.Financial.Invoicing.InvoiceEditComponent

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

let possibleTaxRates = [ 0; 6; 12; 21 ]

type InvoiceInputModel = 
    {
        InvoiceId: Guid
        BuildingId: Guid
        FinancialYear: FinancialYear option
        InvoiceNumber: int option
        Description: string option
        Cost: string option
        VatRate: int
        FinancialCategory: FinancialCategory option
        BookingDate: DateTime
        DistributionKey: DistributionKeyListItem option
        InvoiceDate: DateTimeOffset
        DueDate: DateTimeOffset
        Organization: OrganizationListItem option
        OrganizationBankAccount: BankAccount option
        OrganizationInvoiceNumber: string option //Number @ supplier
        MediaFiles: MediaFile list
        Payments: InvoicePayment list
    }
    static member Init (buildingId: BuildingId): InvoiceInputModel = {
        InvoiceId = Guid.NewGuid()
        BuildingId = buildingId
        BookingDate = DateTime.Today
        FinancialYear = None
        InvoiceNumber = None
        Description = None
        Cost = None
        VatRate = 21
        FinancialCategory = None
        DistributionKey = None
        InvoiceDate = new DateTimeOffset(DateTime.Today)
        DueDate = new DateTimeOffset(DateTime.Today)
        Organization = None
        OrganizationInvoiceNumber = None
        OrganizationBankAccount = None
        MediaFiles = []
        Payments = []
    } 
    static member fromInvoice (invoice: Invoice): InvoiceInputModel = {
        InvoiceId = invoice.InvoiceId
        BuildingId = invoice.BuildingId
        FinancialYear = Some invoice.FinancialYear
        InvoiceNumber = Some invoice.InvoiceNumber
        Description = invoice.Description
        Cost = Some (String.Format("{0:0.00}", invoice.Cost).Replace(".", ","))
        VatRate = invoice.VatRate
        FinancialCategory = Some invoice.FinancialCategory
        BookingDate = invoice.BookingDate
        DistributionKey = Some invoice.DistributionKey
        InvoiceDate = invoice.InvoiceDate
        DueDate = invoice.DueDate
        Organization = Some invoice.Organization
        OrganizationInvoiceNumber = invoice.OrganizationInvoiceNumber
        OrganizationBankAccount = Some invoice.OrganizationBankAccount
        MediaFiles = invoice.MediaFiles
        Payments = invoice.Payments
    }

    member me.Validate (): Result<Invoice, (string * string) list> = 
        let mandatoryToTrial (path: string) (opt: 'T option) =
            match opt with
            | Some x -> Trial.Pass x
            | None -> Trial.ofError (path, "Verplicht veld")

        let validateCost (cost) =
            match cost |> Option.defaultValue "" with
            | x when String.IsNullOrWhiteSpace(x) -> Trial.ofError (nameof me.Cost, "Verplicht veld")
            | x ->
                match Decimal.TryParse(x.Replace(',', '.')) with
                | true, parsed -> Trial.Pass parsed
                | false, _ -> Trial.ofError (nameof me.Cost, "Foutieve formattering")

        trial {
            from financialYear in mandatoryToTrial (nameof me.FinancialYear) me.FinancialYear
            also financialCategory in mandatoryToTrial (nameof me.FinancialCategory) me.FinancialCategory
            also organizationBankAccount in mandatoryToTrial (nameof me.OrganizationBankAccount) me.OrganizationBankAccount
            also distributionKey in mandatoryToTrial (nameof me.DistributionKey) me.DistributionKey
            also organization in mandatoryToTrial (nameof me.Organization) me.Organization
            also cost in validateCost me.Cost
            yield {
                Invoice.InvoiceId = me.InvoiceId
                BuildingId = me.BuildingId
                FinancialYear = financialYear
                InvoiceNumber = me.InvoiceNumber |> Option.defaultValue -1
                Description = me.Description
                Cost = cost
                VatRate = me.VatRate
                FinancialCategory = financialCategory
                Organization = organization
                OrganizationBankAccount = organizationBankAccount
                BookingDate = me.BookingDate
                DistributionKey = distributionKey
                OrganizationInvoiceNumber = me.OrganizationInvoiceNumber
                InvoiceDate = me.InvoiceDate
                DueDate = me.DueDate
                MediaFiles = me.MediaFiles
                Payments = me.Payments
            }
        }
        |> Trial.toResult

type Message =
    | CostChanged of string
    | VatRateChanged of int
    | FinancialYearChanged of FinancialYear
    | FinancialYearsLoaded of FinancialYear list
    | ChangeCategory
    | CategoryChanged of FinancialCategory option
    | CancelChangeFinancialCategory
    | DescriptionChanged of string
    | BookingDateChanged of DateTime
    | ChangeDistributionKey
    | DistributionKeyChanged of DistributionKeyListItem option
    | CancelChangeDistributionKey
    | ChangeOrganization
    | OrganizationChanged of OrganizationListItem option
    | CancelChangeOrganization
    | InvoiceDateChanged of DateTimeOffset
    | DueDateChanged of DateTimeOffset
    | ExternalInvoiceNumberChanged of string
    | OrganizationBankAccountChanged of BankAccount option

    | DistributionKeysLoaded of DistributionKeyListItem list
    | OrganizationsLoaded of OrganizationListItem list
    | RemotingException of exn

    | MediaFileAdded of MediaFile
    | MediaFileRemoved of MediaFile

type State = {
    Invoice: InvoiceInputModel
    FinancialYears: FinancialYear list
    LoadingFinancialYears: bool
    DistributionKeys: DistributionKeyListItem list
    LoadingDistributionKeys: bool
    Organizations: OrganizationListItem list
    LoadingOrganizations: bool
    ShowingFinancialCategoryModal: bool
    ShowingDistributionKeyModal: bool
    ShowingOrganizationModal: bool
    CurrentBuilding: BuildingListItem
    Errors: (string * string) list
    SelectedOrganization: OrganizationListItem option
}

type InvoiceEditComponentProps =
    {|
        Invoice: Invoice option
        CurrentBuilding: BuildingListItem
    |}

let init (props: InvoiceEditComponentProps) =
    let remotingApi = Client.Remoting.getRemotingApi()
    let inputModel =
        match props.Invoice with
        | Some invoice ->
            InvoiceInputModel.fromInvoice (invoice)
        | None ->
            InvoiceInputModel.Init (props.CurrentBuilding.BuildingId)

    {
        Invoice = inputModel
        FinancialYears = []
        LoadingFinancialYears = true
        DistributionKeys = []
        LoadingDistributionKeys = true
        Organizations = []
        LoadingOrganizations = true
        ShowingFinancialCategoryModal = false
        ShowingDistributionKeyModal = false
        ShowingOrganizationModal = false
        Errors = []
        CurrentBuilding = props.CurrentBuilding
        SelectedOrganization = None
    }
    , Cmd.batch [
        Cmd.OfAsync.either remotingApi.GetFinancialYears inputModel.BuildingId FinancialYearsLoaded RemotingException
        Cmd.OfAsync.either remotingApi.GetDistributionKeyListItems inputModel.BuildingId DistributionKeysLoaded RemotingException
        Cmd.OfAsync.either remotingApi.GetOrganizations inputModel.BuildingId OrganizationsLoaded RemotingException
    ]

let update (message: Message) (state: State): State * Cmd<Message> =
    let changeInvoice f state =
        { state with Invoice = f state.Invoice }

    let recalculateValidationErrors (state: State) =
        match state.Errors with
        | [] -> state
        | _errors ->
            match state.Invoice.Validate() with
            | Ok _validated -> state
            | Error validationErrors -> { state with Errors = validationErrors }

    match message with
    | CostChanged x ->
        let newCost = if String.IsNullOrEmpty(x) then None else Some x
        state |> changeInvoice (fun invoice -> { invoice with Cost = newCost })
        , Cmd.none
    | FinancialYearChanged financialYear ->
        state |> changeInvoice (fun invoice -> { invoice with FinancialYear = Some financialYear })
        , Cmd.none
    | DescriptionChanged description ->
        let newDescription = if String.IsNullOrEmpty(description) then None else Some description
        state |> changeInvoice (fun invoice -> { invoice with Description = newDescription })
        , Cmd.none
    | VatRateChanged vatRate ->
        state |> changeInvoice (fun invoice -> { invoice with VatRate = vatRate })
        , Cmd.none
    | ChangeCategory ->
        { state with ShowingFinancialCategoryModal = true }, Cmd.none
    | CategoryChanged financialCategory ->
        { state with ShowingFinancialCategoryModal = false }
        |> changeInvoice (fun invoice -> { invoice with FinancialCategory = financialCategory })
        , Cmd.none
    | CancelChangeFinancialCategory ->
        { state with ShowingFinancialCategoryModal = false }, Cmd.none
    | BookingDateChanged bookingDate ->
        state |> changeInvoice (fun invoice -> { invoice with BookingDate = bookingDate })
        , Cmd.none
    | ChangeDistributionKey ->
        { state with ShowingDistributionKeyModal = true }, Cmd.none
    | DistributionKeyChanged distributionKey ->
        { state with ShowingDistributionKeyModal = false }
        |> changeInvoice (fun invoice -> { invoice with DistributionKey = distributionKey })
        , Cmd.none
    | CancelChangeDistributionKey ->
        { state with ShowingDistributionKeyModal = false }, Cmd.none
    | ChangeOrganization ->
        { state with ShowingOrganizationModal = true }, Cmd.none
    | OrganizationChanged organization ->
        { state with 
            ShowingOrganizationModal = false
            SelectedOrganization = organization }
        |> changeInvoice (fun invoice -> { 
            invoice with 
                Organization = organization
                OrganizationBankAccount = organization |> Option.bind (fun o -> o.BankAccounts |> List.tryHead) 
        })
        , Cmd.none
    | CancelChangeOrganization ->
        { state with ShowingOrganizationModal = false }, Cmd.none
    | InvoiceDateChanged invoiceDate ->
        state |> changeInvoice (fun invoice -> { invoice with InvoiceDate = invoiceDate })
        , Cmd.none
    | DueDateChanged dueDate ->
        state |> changeInvoice (fun invoice -> { invoice with DueDate = dueDate })
        , Cmd.none
    | ExternalInvoiceNumberChanged invoiceNumber ->
        let newInvoiceNumber =
            if String.IsNullOrEmpty(invoiceNumber) 
            then None
            else Some invoiceNumber
        state |> changeInvoice (fun invoice -> { invoice with OrganizationInvoiceNumber = newInvoiceNumber }), Cmd.none
    | OrganizationBankAccountChanged account ->
        state |> changeInvoice (fun invoice -> { invoice with OrganizationBankAccount = account })
        , Cmd.none 
    | FinancialYearsLoaded financialYears ->
        { state with LoadingFinancialYears = false; FinancialYears = financialYears }
        |> changeInvoice (fun invoice -> 
            match invoice.FinancialYear with 
            | None -> { invoice with FinancialYear = financialYears |> List.tryHead } 
            | Some _ -> invoice)
        , Cmd.none
    | DistributionKeysLoaded distributionKeys ->
        { state with LoadingDistributionKeys = false; DistributionKeys = distributionKeys }, Cmd.none
    | OrganizationsLoaded organizations ->
        { 
            state with 
                LoadingOrganizations = false
                Organizations = organizations
                SelectedOrganization = state.Invoice.Organization
        }, Cmd.none
    | RemotingException error ->
        state, showGenericErrorModalCmd error
    | MediaFileAdded mediaFile ->
        state |> changeInvoice (fun i -> { i with MediaFiles = mediaFile::i.MediaFiles })
        , Cmd.none
    | MediaFileRemoved mediaFile ->
        state |> changeInvoice (fun i -> { i with MediaFiles = i.MediaFiles |> List.filter (fun m -> m.FileId = mediaFile.FileId) })
        , Cmd.none

    |> (fun (state, cmd) -> state |> recalculateValidationErrors, cmd)

let inColumn x = div [ Class Bootstrap.col ] [ x ]
let inColumn' colClass x = div [ Class colClass ] [ x ]

let view (financialCategories: FinancialCategory list) (state: State) (dispatch: Message -> unit) =
    let errorFor (path: string) =
        state.Errors |> List.tryPick (fun (p, error) -> if p = path then Some error else None)

    let bankAccountErrorFor (path: string) =
        errorFor (sprintf "%s.%s" (nameof state.Invoice.OrganizationBankAccount) path)

    let vatRateToOption (vatRate: int): FormSelectOption = {
        Key = sprintf "%i%%" (int vatRate)
        Label = sprintf "%i%%" (int vatRate)
        IsSelected = state.Invoice.VatRate = vatRate
    }

    let financialYearToOption (financialYear: FinancialYear): FormSelectOption = {
        Key = sprintf "%A" financialYear.FinancialYearId
        Label = financialYear.Code
        IsSelected = state.Invoice.FinancialYear = Some financialYear
    }

    let vatRateSelection: FormSelect = {
        Identifier = "vatRate"
        OnChanged = (fun e -> VatRateChanged (int (e.TrimEnd('%'))) |> dispatch)
        Options = possibleTaxRates |> List.map vatRateToOption
    }

    let financialYearSelection: FormSelect = {
        Identifier = "financialYear"
        OnChanged = (fun e -> 
            FinancialYearChanged (state.FinancialYears |> List.find (fun year -> string year.FinancialYearId = e)) 
            |> dispatch)
        Options = state.FinancialYears |> List.map financialYearToOption
    }

    let organizationBankAccountSelection: FormSelect = 
        let possibleBankAccounts = 
            match state.SelectedOrganization with
            | Some org -> org.BankAccounts
            | None -> []

        let organizationBankAccountToOption (index: int) (account: BankAccount): FormSelectOption = {
            Key = string index
            Label =
                [ 
                    account.Description
                    account.IBAN 
                ] 
                |> List.filter (String.IsNullOrEmpty >> not) 
                |> String.joinWith " - "
            IsSelected = state.Invoice.OrganizationBankAccount = Some account
        }

        {
            Identifier = "toAccount"
            OnChanged = (fun e -> OrganizationBankAccountChanged (possibleBankAccounts |> List.indexed |> List.tryPick (fun (index, bankAccount) -> if (string index) = e then Some bankAccount else None)) |> dispatch)
            Options = possibleBankAccounts |> List.mapi organizationBankAccountToOption
        }

    let calculateVat (cost: string option) (vatRate: int) = 
        match cost with
        | Some x ->
            match Decimal.TryParse(x) with
            | true, parsed -> Math.Round(parsed * (vatRate |> decimal) / (100.0 |> decimal), 2)
            | false, _ -> 0 |> decimal
        | None ->
            0 |> decimal

    let renderDistributionKeyModal (props: {| SelectedDistributionKey: DistributionKeyListItem option; AllDistributionKeys: DistributionKeyListItem list; Showing: bool |}) =
        BasicModal.render
            {| 
                ModalProps = [
                    IsOpen props.Showing
                    DisableBackgroundClick false
                    OnDismiss (fun _ -> dispatch CancelChangeDistributionKey)
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
                                OnSelectionChanged = fun selection -> Message.DistributionKeyChanged (selection |> List.tryHead) |> dispatch
                                ListItemToString = fun dKey -> dKey.Name
                            |}, "DistributionKeySelectionList")
                    ]
                    Footer [
                        //yield FooterProp.Buttons []
                    ]
                ]
            |}

    let renderOrganizationModal (props: {| SelectedOrganizationId: Guid option; AllOrganizations: OrganizationListItem list; Showing: bool |}) =
        BasicModal.render 
            {| 
                ModalProps = [
                    IsOpen props.Showing
                    DisableBackgroundClick false
                    OnDismiss (fun _ -> dispatch CancelChangeOrganization)
                    Header [
                        HeaderProp.Title "Leverancier selecteren"
                        HeaderProp.HasDismissButton true
                    ]
                    Body [
                        SelectionList.render (
                            {|
                                SelectionMode = SelectionMode.SingleSelect
                                AllItems = props.AllOrganizations |> List.sortBy (fun cat -> cat.Name)
                                SelectedItems = 
                                    match props.SelectedOrganizationId with
                                    | Some selected -> props.AllOrganizations |> List.filter (fun org -> org.OrganizationId = selected)
                                    | None -> []
                                OnSelectionChanged = fun selection -> OrganizationChanged (selection |> List.tryHead) |> dispatch
                                ListItemToString = fun dKey -> dKey.Name
                            |}, "OrganizationSelectionList")
                    ]
                    Footer [
                        //yield FooterProp.Buttons []
                    ]
                ]
            |}

    [
        div [ Class Bootstrap.row ] [
            formGroup [ 
                Label "Boekjaar"
                Select financialYearSelection
                FieldError (errorFor (nameof state.Invoice.FinancialYear))
            ]
            |> inColumn

            formGroup [
                Label "Boekingsdatum"
                Date [
                    Flatpickr.OnChange (fun e -> BookingDateChanged e |> dispatch)
                    Flatpickr.Value (state.Invoice.BookingDate)
                    Flatpickr.SelectionMode Flatpickr.Mode.Single
                    Flatpickr.EnableTimePicker false
                    Flatpickr.DateFormat "d/m/Y"
                ]
                FieldError (errorFor (nameof state.Invoice.BookingDate))
            ]
            |> inColumn

            formGroup [
                Label "Boekhoudkundige rekening"

                let shown =
                    match state.Invoice.FinancialCategory with
                    | Some category -> sprintf "%s - %s" category.Code category.Description
                    | _ -> ""
                Input [
                    Type "text"
                    Style [ Cursor "pointer"; BackgroundColor "unset" ]
                    ReadOnly true
                    OnClick (fun _ -> ChangeCategory |> dispatch)
                    valueOrDefault shown
                ]
                InputAppend [
                    button [ 
                        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]
                        OnClick (fun _ -> ChangeCategory |> dispatch)
                    ] [
                        span [ classes [ FontAwesome.fas; FontAwesome.faSearch ] ] []
                    ]
                ]
                FieldError (errorFor (nameof state.Invoice.FinancialCategory))
            ]
            |> inColumn
        ]
        div [ Class Bootstrap.row ] [
            formGroup [
                Label "Leverancier"
                Input [
                    Type "text"
                    ReadOnly true
                    Style [ BackgroundColor "unset"; Cursor "pointer" ]
                    OnClick (fun _ -> ChangeOrganization |> dispatch)
                    valueOrDefault (state.Invoice.Organization |> Option.map (fun o -> o.Name) |> Option.defaultValue "")
                ]
                InputAppend [
                    button [ 
                        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]
                        OnClick (fun _ -> ChangeOrganization |> dispatch)
                    ] [
                        span [ classes [ FontAwesome.fas; FontAwesome.faSearch ] ] []
                    ]
                ]
                FieldError (errorFor (nameof state.Invoice.Organization))
            ]
            |> inColumn

            match state.Invoice.Organization |> Option.map (fun o -> o.VatNumber), state.Invoice.Organization |> Option.map (fun o -> o.OrganizationNumber) with
            | Some vatNumber, _ ->               
                formGroup [
                    Label "Leverancier BTW nr."
                    Input [
                        Type "text"
                        Disabled true
                        valueOrDefault (vatNumber |> Option.defaultValue "")
                    ]
                ]
            | _, Some orgNr ->
                formGroup [
                    Label "Leverancier ondernemingsnr."
                    Input [
                        Type "text"
                        Disabled true
                        valueOrDefault (orgNr |> Option.defaultValue "")
                    ]
                ]
            | _ ->
                formGroup [
                    Label "Leverancier BTW nr."
                    Input [
                        Type "text"
                        Disabled true
                        valueOrDefault ""
                    ]
                ]
            |> inColumn

        ]
        div [ Class Bootstrap.row ] [
            formGroup [
                Label "Factuurnummer"
                Input [
                    Type "text"
                    OnChange (fun e -> ExternalInvoiceNumberChanged e.Value |> dispatch)
                    valueOrDefault (state.Invoice.OrganizationInvoiceNumber |> Option.defaultValue "")
                ]
                FieldError (errorFor (nameof state.Invoice.InvoiceNumber))
            ]
            |> inColumn
            formGroup [
                Label "Factuurdatum"
                Date [
                    Flatpickr.OnChange (fun e -> InvoiceDateChanged (new DateTimeOffset(e)) |> dispatch)
                    Flatpickr.Value (state.Invoice.InvoiceDate.LocalDateTime)
                    Flatpickr.SelectionMode Flatpickr.Mode.Single
                    Flatpickr.EnableTimePicker false
                    Flatpickr.DateFormat "d/m/Y"
                ]
                FieldError (errorFor (nameof state.Invoice.InvoiceDate))
            ]
            |> inColumn
            formGroup [
                Label "Uiterste betaaldatum"
                Date [
                    Flatpickr.OnChange (fun e -> DueDateChanged (new DateTimeOffset(e)) |> dispatch)
                    Flatpickr.Value (state.Invoice.DueDate.LocalDateTime)
                    Flatpickr.SelectionMode Flatpickr.Mode.Single
                    Flatpickr.EnableTimePicker false
                    Flatpickr.DateFormat "d/m/Y"
                ]
                FieldError (errorFor (nameof state.Invoice.DueDate))
            ]
            |> inColumn
        ]
        div [ Class Bootstrap.row ] [
            formGroup [
                Label "Bedrag (Incl. BTW)"
                InputPrepend [
                    span [ Class Bootstrap.inputGroupText ] [ str "€" ]
                ]
                Input [
                    Type "text"
                    OnChange (fun e -> CostChanged e.Value |> dispatch)
                    valueOrDefault (state.Invoice.Cost |> Option.defaultValue "")
                ]
                FieldError (errorFor (nameof state.Invoice.Cost))
            ]
            |> inColumn
            formGroup [
                Label "BTW tarief"
                Select vatRateSelection
                FieldError (errorFor (nameof state.Invoice.VatRate))
            ]
            |> inColumn
            formGroup [
                Label "BTW totaal"
                Input [
                    Type "text"
                    Disabled true
                    valueOrDefault (calculateVat state.Invoice.Cost state.Invoice.VatRate)
                ]
            ]
            |> inColumn

            formGroup [
                Label "Verdeelsleutel"
                Input [
                    Type "text"
                    Style [ Cursor "pointer"; BackgroundColor "unset" ]
                    ReadOnly true
                    OnClick (fun e -> ChangeDistributionKey |> dispatch)
                    valueOrDefault (state.Invoice.DistributionKey |> Option.map (fun dKey -> dKey.Name) |> Option.defaultValue "")
                ]
                InputAppend [
                    button [ 
                        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]
                        OnClick (fun _ -> ChangeDistributionKey |> dispatch)
                    ] [
                        span [ classes [ FontAwesome.fas; FontAwesome.faSearch ] ] []
                    ]
                ]
                FieldError (errorFor (nameof state.Invoice.DistributionKey))
            ]
            |> inColumn
        ]

        div [ Class Bootstrap.row ] [
            formGroup [
                Label "Naar rekening"
                match state.Invoice.Organization with
                | Some _ ->
                    Select organizationBankAccountSelection
                | None ->
                    OtherChildren [ div [] [ span [ Class Bootstrap.formControl ] [ str "Gelieve eerst een leverancier te selecteren" ] ] ]
                FieldError (errorFor (nameof state.Invoice.OrganizationBankAccount))
            ]
            |> inColumn' Bootstrap.col5

            formGroup [
                Label "IBAN"
                Input [
                    Type "text"
                    MaxLength 64.0
                    Disabled true
                    valueOrDefault (state.Invoice.OrganizationBankAccount |> Option.map (fun account -> account.IBAN) |> Option.defaultValue "")
                ]
                FieldError (bankAccountErrorFor (nameof state.Invoice.OrganizationBankAccount.Value.IBAN))
            ]
            |> inColumn

            formGroup [
                Label "BIC"
                Input [
                    Type "text"
                    MaxLength 11.0
                    Disabled true
                    valueOrDefault (state.Invoice.OrganizationBankAccount |> Option.map (fun account -> account.BIC) |> Option.defaultValue "")
                ]
                FieldError (bankAccountErrorFor (nameof state.Invoice.OrganizationBankAccount.Value.BIC))
            ]
            |> inColumn
        ]

        div [] [
            filePond 
                {|
                    BuildingId = Some state.CurrentBuilding.BuildingId
                    EntityId = state.Invoice.InvoiceId
                    Partition = Partitions.Invoices
                    Options = [
                        FilePondOptions.AllowMultiple true
                        FilePondOptions.MaxFiles 10
                        FilePondOptions.InitialFiles (state.Invoice.MediaFiles |> List.map (fun m -> m.FileId))
                        FilePondOptions.OnProcessFile (fun error filePondFile ->
                            if String.IsNullOrWhiteSpace(error) then
                                filePondFile 
                                |> FilePondFile.toMediaFile Partitions.Invoices (Some state.CurrentBuilding.BuildingId) state.Invoice.InvoiceId
                                |> MediaFileAdded
                                |> dispatch)
                        FilePondOptions.OnRemoveFile (fun error filePondFile ->
                            if String.IsNullOrWhiteSpace(error) then
                                filePondFile
                                |> FilePondFile.toMediaFile Partitions.Invoices (Some state.CurrentBuilding.BuildingId) state.Invoice.InvoiceId
                                |> MediaFileRemoved
                                |> dispatch)
                    ]
                |}
        ]

        FinancialCategorySelectionModal.render 
            {|
                SelectedCategoryId = state.Invoice.FinancialCategory |> Option.map (fun cat -> cat.FinancialCategoryId)
                FinancialCategories = financialCategories 
                Showing = state.ShowingFinancialCategoryModal
                OnCancelSelection = fun () -> dispatch (CancelChangeFinancialCategory)
                OnSelectionChanged = fun (selected) -> dispatch (CategoryChanged selected)
                Filter = Some (fun cat -> cat.Code.StartsWith("6"))
            |}
        renderDistributionKeyModal 
            {| 
                SelectedDistributionKey = state.Invoice.DistributionKey
                AllDistributionKeys = state.DistributionKeys 
                Showing = state.ShowingDistributionKeyModal
            |}
        renderOrganizationModal 
            {|
                SelectedOrganizationId = state.Invoice.Organization |> Option.map (fun org -> org.OrganizationId)
                AllOrganizations = state.Organizations 
                Showing = state.ShowingOrganizationModal
            |}
    ]
    |> fragment []