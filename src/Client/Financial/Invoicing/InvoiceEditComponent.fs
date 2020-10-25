module Client.Financial.Invoicing.InvoiceEditComponent

open System
open Client.Upload
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Library
open Client.Components
open Client.Components.BasicModal
open Client.Components.SelectionList
open Shared.Read
open Shared.Library
open Shared.Trial
open Shared.Trial.Control
open Shared.MediaLibrary

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
        InvoiceDate: DateTime
        DueDate: DateTime
        Organization: OrganizationListItem option
        OrganizationBankAccount: BankAccount option
        OrganizationInvoiceNumber: string option //Number @ supplier
        MediaFiles: MediaFile list
    }
    static member init (buildingId: BuildingId): InvoiceInputModel = {
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
        InvoiceDate = DateTime.Today
        DueDate = DateTime.Today
        Organization = None
        OrganizationInvoiceNumber = None
        OrganizationBankAccount = None
        MediaFiles = []
    } 
    static member fromInvoice (invoice: Invoice): InvoiceInputModel = {
        InvoiceId = invoice.InvoiceId
        BuildingId = invoice.BuildingId
        FinancialYear = Some invoice.FinancialYear
        InvoiceNumber = Some invoice.InvoiceNumber
        Description = invoice.Description
        Cost = Some (invoice.Cost.ToString().Replace(".", ","))
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
                MediaFiles = []
            }
        }
        |> Trial.toResult

type Message =
    | CostChanged of string
    | VatRateChanged of int
    | FinancialYearChanged of FinancialYear
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
    | InvoiceDateChanged of DateTime
    | DueDateChanged of DateTime
    | ExternalInvoiceNumberChanged of string
    | OrganizationBankAccountChanged of BankAccount option
    | OrganizationAccountBICChanged of string
    | OrganizationAccountIBANChanged of string

    | FinancialYearsLoaded of FinancialYear list
    | FinancialCategoriesLoaded of FinancialCategory list
    | DistributionKeysLoaded of DistributionKeyListItem list
    | OrganizationsLoaded of OrganizationListItem list
    | RemotingException of exn

    | MediaFileAdded of MediaFile
    | MediaFileRemoved of MediaFile

    | AddPayment
    | CancelPayment

type State = {
    Invoice: InvoiceInputModel
    FinancialYears: FinancialYear list
    LoadingFinancialYears: bool
    FinancialCategories: FinancialCategory list
    LoadingFinancialCategories: bool
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
    ShowingPaymentModal: bool
}

let init (invoice: Invoice option, currentBuilding: BuildingListItem) =
    let remotingApi = Client.Remoting.getRemotingApi()
    let inputModel =
        match invoice with
        | Some invoice ->
            InvoiceInputModel.fromInvoice (invoice)
        | None ->
            InvoiceInputModel.init (currentBuilding.BuildingId)

    {
        Invoice = inputModel
        FinancialYears = []
        LoadingFinancialYears = true
        FinancialCategories = []
        LoadingFinancialCategories = true
        DistributionKeys = []
        LoadingDistributionKeys = true
        Organizations = []
        LoadingOrganizations = true
        ShowingFinancialCategoryModal = false
        ShowingDistributionKeyModal = false
        ShowingOrganizationModal = false
        Errors = []
        CurrentBuilding = currentBuilding
        SelectedOrganization = None
        ShowingPaymentModal = false
    }
    , Cmd.batch [
        Cmd.OfAsync.either remotingApi.GetFinancialYears inputModel.BuildingId FinancialYearsLoaded RemotingException
        Cmd.OfAsync.either remotingApi.GetFinancialCategories inputModel.BuildingId FinancialCategoriesLoaded RemotingException
        Cmd.OfAsync.either remotingApi.GetDistributionKeyListItems inputModel.BuildingId DistributionKeysLoaded RemotingException
        Cmd.OfAsync.either remotingApi.GetOrganizations inputModel.BuildingId OrganizationsLoaded RemotingException
    ]

let update (message: Message) (state: State): State * Cmd<Message> =
    let changeInvoice f state =
        { state with Invoice = f state.Invoice }

    let removeError path state =
        { state with Errors = (state.Errors |> List.filter (fun (ePath, _) -> ePath <> path)) }

    match message with
    | CostChanged x ->
        let newCost = if String.IsNullOrEmpty(x) then None else Some x
        state 
        |> changeInvoice (fun invoice -> { invoice with Cost = newCost })
        |> removeError (nameof state.Invoice.Cost)
        , Cmd.none
    | FinancialYearChanged financialYear ->
        state
        |> changeInvoice (fun invoice -> { invoice with FinancialYear = Some financialYear })
        |> removeError (nameof state.Invoice.FinancialYear)
        , Cmd.none
    | DescriptionChanged description ->
        let newDescription = if String.IsNullOrEmpty(description) then None else Some description
        state
        |> changeInvoice (fun invoice -> { invoice with Description = newDescription })
        |> removeError (nameof state.Invoice.Description)
        , Cmd.none
    | VatRateChanged vatRate ->
        printf "New vat rate: %i" vatRate
        state
        |> changeInvoice (fun invoice -> { invoice with VatRate = vatRate })
        |> removeError (nameof state.Invoice.VatRate)
        , Cmd.none
    | ChangeCategory ->
        { state with ShowingFinancialCategoryModal = true }, Cmd.none
    | CategoryChanged financialCategory ->
        { state with ShowingFinancialCategoryModal = false }
        |> changeInvoice (fun invoice -> { invoice with FinancialCategory = financialCategory })
        |> removeError (nameof state.Invoice.FinancialCategory)
        , Cmd.none
    | CancelChangeFinancialCategory ->
        { state with ShowingFinancialCategoryModal = false }, Cmd.none
    | BookingDateChanged bookingDate ->
        state
        |> changeInvoice (fun invoice -> { invoice with BookingDate = bookingDate })
        |> removeError (nameof state.Invoice.BookingDate)
        , Cmd.none
    | ChangeDistributionKey ->
        { state with ShowingDistributionKeyModal = true }, Cmd.none
    | DistributionKeyChanged distributionKey ->
        { state with ShowingDistributionKeyModal = false }
        |> changeInvoice (fun invoice -> { invoice with DistributionKey = distributionKey })
        |> removeError (nameof state.Invoice.DistributionKey)
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
        |> removeError (nameof state.Invoice.Organization)
        , Cmd.none
    | CancelChangeOrganization ->
        { state with ShowingOrganizationModal = false }, Cmd.none
    | InvoiceDateChanged invoiceDate ->
        state
        |> changeInvoice (fun invoice -> { invoice with InvoiceDate = invoiceDate })
        |> removeError (nameof state.Invoice.InvoiceDate)
        , Cmd.none
    | DueDateChanged dueDate ->
        state
        |> changeInvoice (fun invoice -> { invoice with DueDate = dueDate })
        |> removeError (nameof state.Invoice.DueDate)
        , Cmd.none
    | ExternalInvoiceNumberChanged invoiceNumber ->
        let newInvoiceNumber =
            if String.IsNullOrEmpty(invoiceNumber) 
            then None
            else Some invoiceNumber
        state |> changeInvoice (fun invoice -> { invoice with OrganizationInvoiceNumber = newInvoiceNumber }), Cmd.none
    | OrganizationBankAccountChanged account ->
        state 
        |> changeInvoice (fun invoice -> { invoice with OrganizationBankAccount = account })
        |> removeError (nameof state.Invoice.OrganizationBankAccount)
        , Cmd.none
    | OrganizationAccountBICChanged newBic ->
        let account = 
            state.Invoice.OrganizationBankAccount 
            |> Option.defaultValue { BankAccount.Init () with Description = "Ander" }
        let newBic' = if String.IsNullOrEmpty(newBic) then "" else newBic

        state
        |> changeInvoice (fun invoice -> { invoice with OrganizationBankAccount = Some { account with BIC = newBic' } })
        , Cmd.none
    | OrganizationAccountIBANChanged newIban ->
        let account = 
            state.Invoice.OrganizationBankAccount 
            |> Option.defaultValue { BankAccount.Init () with Description = "Ander" }
        let newIban' = if String.IsNullOrEmpty(newIban) then "" else newIban

        state
        |> changeInvoice (fun invoice -> { invoice with OrganizationBankAccount = Some { account with IBAN = newIban' } })
        , Cmd.none        
    | FinancialYearsLoaded financialYears ->
        { state with LoadingFinancialYears = false; FinancialYears = financialYears }, Cmd.none
    | FinancialCategoriesLoaded financialCategories ->
        { state with LoadingFinancialCategories = false; FinancialCategories = financialCategories }, Cmd.none 
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
    | AddPayment ->
        { state with ShowingPaymentModal = true }, Cmd.none
    | CancelPayment ->
        { state with ShowingPaymentModal = false }, Cmd.none
    | MediaFileAdded mediaFile ->
        state |> changeInvoice (fun i -> { i with MediaFiles = mediaFile::i.MediaFiles })
        , Cmd.none
    | MediaFileRemoved mediaFile ->
        state |> changeInvoice (fun i -> { i with MediaFiles = i.MediaFiles |> List.filter (fun m -> m.FileId = mediaFile.FileId) })
        , Cmd.none

let inColumn x = div [ Class Bootstrap.col ] [ x ]

let view (state: State) (dispatch: Message -> unit) =
    let errorFor (path: string) =
        state.Errors |> List.tryPick (fun (p, error) -> if p = path then Some error else None)

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

    let organizationBankAccountToOption (account: BankAccount): FormSelectOption = {
        Key = account.Description
        Label =
            [ 
                account.Description
                account.IBAN 
            ] 
            |> List.filter (String.IsNullOrEmpty >> not) 
            |> String.JoinWith " - "
        IsSelected = state.Invoice.OrganizationBankAccount = Some account
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

    let parseAccount (s: string, accounts: BankAccount list): BankAccount option =
        accounts
        |> List.tryFind (fun account -> account.BIC = s)

    let organizationBankAccountSelection: FormSelect = 
        let possibleBankAccounts = 
            match state.SelectedOrganization with
            | Some org ->
                org.BankAccounts @ [ { (BankAccount.Init ()) with Description = "Ander" } ]
            | None ->
                []

        {
            Identifier = "toAccount"
            OnChanged = (fun e -> OrganizationBankAccountChanged (parseAccount (e, possibleBankAccounts)) |> dispatch)
            Options = possibleBankAccounts |> List.map organizationBankAccountToOption
        }

    let calculateVat (cost: string option) (vatRate: int) = 
        match cost with
        | Some x ->
            match Decimal.TryParse(x) with
            | true, parsed -> Math.Round(parsed * (vatRate |> decimal) / (100.0 |> decimal), 2)
            | false, _ -> 0 |> decimal
        | None ->
            0 |> decimal

    let renderFinancialCategoryModal =
        FunctionComponent.Of((fun (props: {| SelectedCategoryCode: string option; FinancialCategories: FinancialCategory list; Showing: bool |}) ->
            BasicModal.render 
                {| 
                    ModalProps = [
                        IsOpen props.Showing
                        DisableBackgroundClick false
                        OnDismiss (fun _ -> dispatch CancelChangeFinancialCategory)
                        Header [ HeaderProp.HasDismissButton true ]
                        Body [
                            SelectionList.render (
                                {|
                                    SelectionMode = SelectionMode.SingleSelect
                                    //Only financial categories starting with 6 should be shown.
                                    AllItems = props.FinancialCategories |> List.filter (fun cat -> cat.Code.StartsWith("6")) |> List.sortBy (fun cat -> cat.Code)
                                    SelectedItems = 
                                        match props.SelectedCategoryCode with
                                        | Some selected -> props.FinancialCategories |> List.filter (fun cat -> cat.Code = selected)
                                        | None -> []
                                    OnSelectionChanged = fun selection -> Message.CategoryChanged (selection |> List.tryHead) |> dispatch
                                    DisplayListItem = 
                                        (fun financialCategory -> 
                                            [
                                                span [ Key financialCategory.Code ] [ str financialCategory.Code ]
                                                str " - "
                                                span [ Key (financialCategory.Code + "1") ] [ str financialCategory.Description ]
                                            ] 
                                            |> ofList)
                                |}, "FinancialCategorySelectionList")
                        ]
                        Footer [
                            //yield FooterProp.Buttons []
                        ]
                    ]           
                |}), "FinancialCategoryModal", equalsButFunctions)

    let renderDistributionKeyModal =
        FunctionComponent.Of((fun (props: {| SelectedDistributionKey: DistributionKeyListItem option; AllDistributionKeys: DistributionKeyListItem list; Showing: bool |}) ->
            BasicModal.render 
                {| 
                    ModalProps = [
                        IsOpen props.Showing
                        DisableBackgroundClick false
                        OnDismiss (fun _ -> dispatch CancelChangeDistributionKey)
                        Header [ HeaderProp.HasDismissButton true ]
                        Body [
                            SelectionList.render (
                                {|
                                    SelectionMode = SelectionMode.SingleSelect
                                    AllItems = props.AllDistributionKeys |> List.sortBy (fun cat -> cat.Name)
                                    SelectedItems = [ props.SelectedDistributionKey ] |> List.choose id
                                    OnSelectionChanged = fun selection -> Message.DistributionKeyChanged (selection |> List.tryHead) |> dispatch
                                    DisplayListItem = fun dKey -> str dKey.Name
                                |}, "DistributionKeySelectionList")
                        ]
                        Footer [
                            //yield FooterProp.Buttons []
                        ]
                    ]
                |}), "DistributionKeyModal", equalsButFunctions)

    let renderOrganizationModal =
        FunctionComponent.Of((fun (props: {| SelectedOrganizationId: Guid option; AllOrganizations: OrganizationListItem list; Showing: bool |}) ->
            BasicModal.render 
                {| 
                    ModalProps = [
                        IsOpen props.Showing
                        DisableBackgroundClick false
                        OnDismiss (fun _ -> dispatch CancelChangeOrganization)
                        Header [ HeaderProp.HasDismissButton true ]
                        Body [
                            SelectionList.render (
                                {|
                                    SelectionMode = SelectionMode.SingleSelect
                                    AllItems = props.AllOrganizations |> List.sortBy (fun cat -> cat.Name)
                                    SelectedItems = 
                                        match props.SelectedOrganizationId with
                                        | Some selected -> props.AllOrganizations |> List.filter (fun org -> org.OrganizationId = selected)
                                        | None -> []
                                    OnSelectionChanged = fun selection -> Message.OrganizationChanged (selection |> List.tryHead) |> dispatch
                                    DisplayListItem = fun dKey -> str dKey.Name
                                |}, "OrganizationSelectionList")
                        ]
                        Footer [
                            //yield FooterProp.Buttons []
                        ]
                    ]
                |}), "OrganizationModal", equalsButFunctions)

    let renderPaymentModal (isOpen: {| Showing: bool |}) =
        BasicModal.render 
            {|
                ModalProps = [
                    ModalProp.IsOpen isOpen.Showing
                    OnDismiss (fun _ -> CancelPayment |> dispatch)
                    ModalProp.Header [
                        HeaderProp.HasDismissButton true
                        HeaderProp.Title "Betaling"
                    ]
                    ModalProp.Body [
                        div [] [ str "Deze pagina is nog niet beschikbaar" ]
                    ]
                    ModalProp.Footer [
                        FooterProp.ShowDismissButton (Some "Annuleren")
                    ]
                ]
            |}

    div [] [
        div [ Class Bootstrap.row ] [
            formGroup [ 
                Label "Boekjaar"
                Select financialYearSelection
                FormError (errorFor (nameof state.Invoice.FinancialYear))
            ]
            |> inColumn

            formGroup [
                Label "Boekingsdatum"
                Date [
                    Flatpickr.OnChange (fun e -> BookingDateChanged e |> dispatch)
                    Flatpickr.Value (state.Invoice.BookingDate)
                    Flatpickr.SelectionMode Flatpickr.Mode.Single
                    Flatpickr.EnableTimePicker false
                ]
                FormError (errorFor (nameof state.Invoice.BookingDate))
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
            ]
            |> inColumn
        ]
        div [ Class Bootstrap.row ] [
            formGroup [
                Label "Leverancier"
                Input [
                    Type "text"
                    Style [ Cursor "pointer"; BackgroundColor "unset" ]
                    ReadOnly true
                    OnClick (fun _ -> ChangeOrganization |> dispatch)
                    valueOrDefault (state.Invoice.Organization |> Option.map (fun o -> o.Name))
                ]
                FormError (errorFor (nameof state.Invoice.Organization))
            ]
            |> inColumn

            match state.Invoice.Organization |> Option.map (fun o -> o.VatNumber), state.Invoice.Organization |> Option.map (fun o -> o.OrganizationNumber) with
            | Some vatNumber, _ ->               
                formGroup [
                    Label "Leverancier BTW nr."
                    Input [
                        Type "text"
                        Disabled true
                        valueOrDefault vatNumber
                    ]
                ]
            | _, Some orgNr ->
                formGroup [
                    Label "Leverancier ondernemingsnr."
                    Input [
                        Type "text"
                        Disabled true
                        valueOrDefault orgNr
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
                    valueOrDefault (state.Invoice.OrganizationInvoiceNumber)
                ]
                FormError (errorFor (nameof state.Invoice.InvoiceNumber))
            ]
            |> inColumn

            formGroup [
                Label "Factuurdatum"
                Date [
                    Flatpickr.OnChange (fun e -> InvoiceDateChanged e |> dispatch)
                    Flatpickr.Value (state.Invoice.InvoiceDate)
                    Flatpickr.SelectionMode Flatpickr.Mode.Single
                    Flatpickr.EnableTimePicker false
                ]
                FormError (errorFor (nameof state.Invoice.InvoiceDate))
            ]
            |> inColumn

            formGroup [
                Label "Uiterste betaaldatum"
                Date [
                    Flatpickr.OnChange (fun e -> DueDateChanged e |> dispatch)
                    Flatpickr.Value (state.Invoice.DueDate)
                    Flatpickr.SelectionMode Flatpickr.Mode.Single
                    Flatpickr.EnableTimePicker false
                ]
            ]
            |> inColumn
        ]
        div [ Class Bootstrap.row ] [
            formGroup [
                Label "Bedrag (Incl. BTW)"
                Input [
                    Type "text"
                    OnChange (fun e -> CostChanged e.Value |> dispatch)
                    valueOrDefault (state.Invoice.Cost)
                ]
                FormError (errorFor (nameof state.Invoice.Cost))
            ]
            |> inColumn

            formGroup [
                Label "BTW tarief"
                Select vatRateSelection
                FormError (errorFor (nameof state.Invoice.VatRate))
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
                FormError (errorFor (nameof state.Invoice.OrganizationBankAccount))
            ]
            |> inColumn

            let noOrgFound = state.Invoice.Organization.IsNone
            let orgBankAccountSelected = 
                match state.Invoice.OrganizationBankAccount, state.Invoice.Organization |> Option.map (fun o -> o.BankAccounts) with
                | Some bankAccount, Some bankAccounts -> bankAccounts |> List.contains bankAccount
                | _ -> false

            formGroup [
                Label "IBAN"
                Input [
                    Type "text"
                    MaxLength 64.0
                    Disabled (noOrgFound || orgBankAccountSelected)
                    valueOrDefault (state.Invoice.OrganizationBankAccount |> Option.map (fun account -> account.IBAN))
                ]
                FormError (errorFor (nameof state.Invoice.OrganizationBankAccount))
            ]
            |> inColumn

            formGroup [
                Label "BIC"
                Input [
                    Type "text"
                    MaxLength 11.0
                    Disabled (noOrgFound || orgBankAccountSelected)
                    valueOrDefault (state.Invoice.OrganizationBankAccount |> Option.map (fun account -> account.BIC))
                ]
                FormError (errorFor (nameof state.Invoice.OrganizationBankAccount))
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
                            printf "%A" error
                            filePondFile 
                            |> FilePondFile.toMediaFile Partitions.Invoices (Some state.CurrentBuilding.BuildingId) state.Invoice.InvoiceId
                            |> MediaFileAdded
                            |> dispatch)
                        FilePondOptions.OnRemoveFile (fun error filePondFile ->
                            printf "%A" error
                            filePondFile
                            |> FilePondFile.toMediaFile Partitions.Invoices (Some state.CurrentBuilding.BuildingId) state.Invoice.InvoiceId
                            |> MediaFileRemoved
                            |> dispatch)
                    ]
                |}
        ]

        div [ classes [ Bootstrap.card; Bootstrap.bgLight ] ] [
            div [ Class Bootstrap.cardBody ] [
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnPrimary ] 
                    OnClick (fun _ -> AddPayment |> dispatch)
                ][
                    i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                    str " "
                    str "Betaling toevoegen"
                ]
            ]
        ]


        renderFinancialCategoryModal 
            {|
                SelectedCategoryCode = state.Invoice.FinancialCategory |> Option.map (fun cat -> cat.Code)
                FinancialCategories = state.FinancialCategories 
                Showing = state.ShowingFinancialCategoryModal
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
        renderPaymentModal 
            {|
                Showing = state.ShowingPaymentModal
            |}
    ]