module Client.Financial.CostDiary.InvoiceEditComponent

open System
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

let possibleTaxRates = [ 0.0; 6.0; 12.0; 21.0 ]

let validateDateTimeOffset (path: string) (s: string option): Trial<DateTime option, string * string> =
    let s' = s |> Option.defaultValue ""
    match s' with
    | x when String.IsNullOrEmpty(x) -> Trial.Pass None
    | x ->
        match x.Split('/') with
        | [| dayStr; monthStr; yearStr |] -> 
            match Int32.TryParse(dayStr), Int32.TryParse(monthStr), Int32.TryParse(yearStr) with
            | (true, day), (true, month), (true, year) ->
                Trial.Pass (Some (new DateTime(year, month, day)))
            | _ ->
                Trial.ofError (path, "Ongeldige dag, maand of jaar ingegeven")
        | _ -> Trial.ofError (path, "Verwacht formaat: dd/MM/yyyy")

let validateMandatoryDateTimeOffset (path: string) (s: string option) =
    let validated = validateDateTimeOffset path s
    match validated with
    | Trial.Pass None -> Trial.ofError (path, "Verplicht veld")
    | Trial.Pass (Some x) -> Trial.Pass x
    | Trial.Fail errors -> Trial.Fail errors

type InvoiceInputModel = 
    {
        InvoiceId: Guid
        BuildingId: Guid
        FinancialYear: FinancialYear option
        InvoiceNumber: int option
        Description: string option
        Cost: string option
        VatRate: float
        CategoryCode: string option
        CategoryDescription: string option
        FromAccount: BankAccount option
        ToAccount: BankAccount option
        BookingDate: string option //Date when booked
        DistributionKey: DistributionKeyListItem option
        OrganizationId: Guid option
        OrganizationName: string option
        OrganizationNumber: string option
        OrganizationVatNumber: string option        
        ExternalInvoiceNumber: string option //Number @ supplier
        InvoiceDate: string option //Date on the invoice
        DueDate: string option //Due date of the invoice
        PaymentIds: Guid list
    }
    static member init (buildingId: BuildingId): InvoiceInputModel = {
        InvoiceId = Guid.NewGuid()
        BuildingId = buildingId
        FinancialYear = None
        InvoiceNumber = None
        Description = None
        Cost = None
        VatRate = 21.0
        CategoryCode = None
        CategoryDescription = None
        BookingDate = Some (DateTimeOffset.Now.ToString("yyyy-MM-dd"))
        DistributionKey = None
        OrganizationId = None
        OrganizationName = None
        OrganizationNumber = None
        OrganizationVatNumber = None
        ExternalInvoiceNumber = None
        InvoiceDate = Some (DateTimeOffset.Now.ToString("yyyy-MM-dd"))
        DueDate = None
        PaymentIds = []
        FromAccount = None
        ToAccount = None
    } 
    static member fromInvoice (invoice: Invoice): InvoiceInputModel = {
        InvoiceId = invoice.InvoiceId
        BuildingId = invoice.BuildingId
        FinancialYear = Some invoice.FinancialYear
        InvoiceNumber = Some invoice.InvoiceNumber
        Description = invoice.Description
        Cost = Some (invoice.Cost.ToString().Replace(".", ","))
        VatRate = invoice.VatRate
        CategoryCode = Some invoice.CategoryCode
        CategoryDescription = Some invoice.CategoryDescription
        BookingDate = Some (invoice.BookingDate.ToString("yyyy-MM-dd"))
        DistributionKey = Some invoice.DistributionKey
        OrganizationId = Some invoice.OrganizationId
        OrganizationName = Some invoice.OrganizationName
        OrganizationNumber = invoice.OrganizationNumber
        OrganizationVatNumber = invoice.OrganizationVatNumber
        ExternalInvoiceNumber = invoice.ExternalInvoiceNumber
        InvoiceDate = Some (invoice.InvoiceDate.ToString("yyyy-MM-dd"))
        DueDate = Some (invoice.DueDate.ToString("yyyy-MM-dd"))
        PaymentIds = invoice.PaymentIds
        FromAccount = Some { BankAccountType = invoice.FromBankAccountType; BIC = invoice.FromBankAccountBIC; IBAN = invoice.FromBankAccountIBAN }
        ToAccount = Some { BankAccountType = ""; BIC = invoice.ToBankAccountBIC; IBAN = invoice.ToBankAccountIBAN }
    }

    member me.Validate (): Result<Invoice, (string * string) list> = 
        let mandatoryToTrial (path: string) (opt: 'T option) =
            match opt with
            | Some x -> Trial.Pass x
            | None -> Trial.ofError (path, "Verplicht veld")

        let validateCost () =
            match me.Cost |> Option.defaultValue "" with
            | x when String.IsNullOrWhiteSpace(x) -> Trial.ofError (nameof me.Cost, "Verplicht veld")
            | x ->
                match Double.TryParse(x.Replace(',', '.')) with
                | true, parsed -> Trial.Pass parsed
                | false, _ -> Trial.ofError (nameof me.Cost, "Foutieve formattering")

        trial {
            from financialYear in mandatoryToTrial (nameof me.FinancialYear) me.FinancialYear
            also categoryCode in mandatoryToTrial (nameof me.CategoryCode) me.CategoryCode
            also categoryDescription in mandatoryToTrial (nameof me.CategoryDescription) me.CategoryDescription
            also fromAccount in mandatoryToTrial (nameof me.FromAccount) me.FromAccount
            also toAccountBic in mandatoryToTrial (nameof me.ToAccount) (me.ToAccount |> Option.map (fun toAccount -> toAccount.BIC))
            also toAccountIban in mandatoryToTrial (nameof me.ToAccount) (me.ToAccount |> Option.map (fun toAccount -> toAccount.IBAN))
            also bookingDate in validateMandatoryDateTimeOffset (nameof me.BookingDate) me.BookingDate
            also distributionKey in mandatoryToTrial (nameof me.DistributionKey) me.DistributionKey
            also organizationId in mandatoryToTrial (nameof me.OrganizationId) me.OrganizationId
            also organizationName in mandatoryToTrial (nameof me.OrganizationName) me.OrganizationName
            also invoiceDate in validateMandatoryDateTimeOffset (nameof me.InvoiceDate) me.InvoiceDate
            also dueDate in validateMandatoryDateTimeOffset (nameof me.DueDate) me.DueDate
            also cost in validateCost ()
            yield {
                InvoiceId = me.InvoiceId
                BuildingId = me.BuildingId
                FinancialYear = financialYear
                InvoiceNumber = me.InvoiceNumber |> Option.defaultValue -1
                Description = me.Description
                Cost = cost
                VatRate = me.VatRate
                CategoryCode = categoryCode
                CategoryDescription = categoryDescription
                FromBankAccountType = fromAccount.BankAccountType
                FromBankAccountBIC = fromAccount.BIC
                FromBankAccountIBAN = fromAccount.IBAN
                ToBankAccountBIC = toAccountBic
                ToBankAccountIBAN = toAccountIban
                BookingDate = bookingDate
                DistributionKey = distributionKey
                OrganizationId = organizationId
                OrganizationName = organizationName
                OrganizationNumber = me.OrganizationNumber
                OrganizationVatNumber = me.OrganizationVatNumber
                ExternalInvoiceNumber = me.ExternalInvoiceNumber
                InvoiceDate = invoiceDate
                DueDate = dueDate
                PaymentIds = me.PaymentIds
            }
        }
        |> Trial.toResult

type Message =
    | CostChanged of string
    | VatRateChanged of float
    | FinancialYearChanged of FinancialYear
    | ChangeCategory
    | CategoryChanged of FinancialCategory option
    | CancelChangeFinancialCategory
    | DescriptionChanged of string
    | BookingDateChanged of string
    | ChangeDistributionKey
    | DistributionKeyChanged of DistributionKeyListItem option
    | CancelChangeDistributionKey
    | ChangeOrganization
    | OrganizationChanged of OrganizationListItem option
    | CancelChangeOrganization
    | InvoiceDateChanged of string
    | DueDateChanged of string
    | ExternalInvoiceNumberChanged of string
    | FromAccountChanged of BankAccount option
    | ToAccountChanged of BankAccount option
    | ToAccountBICChanged of string
    | ToAccountIBANChanged of string

    | FinancialYearsLoaded of FinancialYear list
    | FinancialCategoriesLoaded of FinancialCategory list
    | DistributionKeysLoaded of DistributionKeyListItem list
    | OrganizationsLoaded of OrganizationListItem list
    | RemotingException of exn

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
        printf "New vat rate: %f" vatRate
        state
        |> changeInvoice (fun invoice -> { invoice with VatRate = vatRate })
        |> removeError (nameof state.Invoice.VatRate)
        , Cmd.none
    | ChangeCategory ->
        { state with ShowingFinancialCategoryModal = true }, Cmd.none
    | CategoryChanged financialCategory ->
        { state with ShowingFinancialCategoryModal = false }
        |> changeInvoice (fun invoice -> { 
            invoice with 
                CategoryCode = financialCategory |> Option.map (fun cat -> cat.Code) 
                CategoryDescription = financialCategory |> Option.map (fun cat -> cat.Description)
        })
        |> removeError (nameof state.Invoice.CategoryCode)
        , Cmd.none
    | CancelChangeFinancialCategory ->
        { state with ShowingFinancialCategoryModal = false }, Cmd.none
    | BookingDateChanged bookingDate ->
        state
        |> changeInvoice (fun invoice -> { invoice with BookingDate = Some bookingDate })
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
        { state with ShowingOrganizationModal = false; SelectedOrganization = organization }
        |> changeInvoice (fun invoice -> 
            {
                invoice with 
                    OrganizationId = organization |> Option.map (fun o -> o.OrganizationId)
                    OrganizationName = organization |> Option.map (fun o -> o.Name)
                    OrganizationNumber = organization |> Option.bind (fun o -> o.OrganizationNumber)
                    OrganizationVatNumber = organization |> Option.bind (fun o -> o.VatNumber)
            })
        |> removeError (nameof state.Invoice.OrganizationId)
        , Cmd.none
    | CancelChangeOrganization ->
        { state with ShowingOrganizationModal = false }, Cmd.none
    | InvoiceDateChanged invoiceDate ->
        let newInvoiceDate = if String.IsNullOrWhiteSpace(invoiceDate) then None else Some invoiceDate
        state
        |> changeInvoice (fun invoice -> { invoice with InvoiceDate = newInvoiceDate })
        |> removeError (nameof state.Invoice.InvoiceDate)
        , Cmd.none
    | DueDateChanged dueDate ->
        let newDueDate = if String.IsNullOrWhiteSpace(dueDate) then None else Some dueDate
        state
        |> changeInvoice (fun invoice -> { invoice with DueDate = newDueDate })
        |> removeError (nameof state.Invoice.DueDate)
        , Cmd.none
    | ExternalInvoiceNumberChanged invoiceNumber ->
        let newInvoiceNumber =
            if String.IsNullOrEmpty(invoiceNumber) 
            then None
            else Some invoiceNumber
        state |> changeInvoice (fun invoice -> { invoice with ExternalInvoiceNumber = newInvoiceNumber }), Cmd.none
    | FromAccountChanged fromAccount ->
        state 
        |> changeInvoice (fun invoice -> { invoice with FromAccount = fromAccount })        
        |> removeError (nameof state.Invoice.FromAccount)
        , Cmd.none
    | ToAccountChanged toAccount ->
        state 
        |> changeInvoice (fun invoice -> { invoice with ToAccount = toAccount })
        |> removeError (nameof state.Invoice.ToAccount)
        , Cmd.none
    | ToAccountBICChanged newBic ->
        let toAccount = state.Invoice.ToAccount |> Option.defaultValue { BankAccountType = ""; BIC = ""; IBAN = "" }
        let newBic' = if String.IsNullOrEmpty(newBic) then "" else newBic

        state
        |> changeInvoice (fun invoice -> { invoice with ToAccount = Some { toAccount with BIC = newBic' } })
        , Cmd.none
    | ToAccountIBANChanged newIban ->
        let toAccount = state.Invoice.ToAccount |> Option.defaultValue { BankAccountType = ""; BIC = ""; IBAN = "" }
        let newIban' = if String.IsNullOrEmpty(newIban) then "" else newIban

        state
        |> changeInvoice (fun invoice -> { invoice with ToAccount = Some { toAccount with IBAN = newIban' } })
        , Cmd.none        
    | FinancialYearsLoaded financialYears ->
        { state with LoadingFinancialYears = false; FinancialYears = financialYears }, Cmd.none
    | FinancialCategoriesLoaded financialCategories ->
        { state with LoadingFinancialCategories = false; FinancialCategories = financialCategories }, Cmd.none 
    | DistributionKeysLoaded distributionKeys ->
        { state with LoadingDistributionKeys = false; DistributionKeys = distributionKeys }, Cmd.none
    | OrganizationsLoaded organizations ->
        { state with 
            LoadingOrganizations = false
            Organizations = organizations
            SelectedOrganization = 
                match state.Invoice.OrganizationId with
                | Some organizationId -> organizations |> List.tryFind (fun org -> org.OrganizationId = organizationId) 
                | None -> None
        }, Cmd.none
    | RemotingException error ->
        state, showGenericErrorModalCmd error

let inColumn x = div [ Class Bootstrap.col ] [ x ]

let view (state: State) (dispatch: Message -> unit) =
    let errorFor (path: string) =
        state.Errors |> List.tryPick (fun (p, error) -> if p = path then Some error else None)

    let vatRateToOption (vatRate: float): FormSelectOption = {
        Key = sprintf "%i%%" (int vatRate)
        Label = sprintf "%i%%" (int vatRate)
        IsSelected = state.Invoice.VatRate = vatRate
    }

    let financialYearToOption (financialYear: FinancialYear): FormSelectOption = {
        Key = sprintf "%A" financialYear.FinancialYearId
        Label = financialYear.Code
        IsSelected = state.Invoice.FinancialYear = Some financialYear
    }

    let fromAccountToOption (account: BankAccount): FormSelectOption = {
        Key = account.IBAN
        Label = sprintf "%s - %s" account.BankAccountType account.IBAN
        IsSelected = state.Invoice.FromAccount = Some account
    }

    let toAccountToOption (account: BankAccount): FormSelectOption = {
        Key = account.IBAN
        Label = sprintf "%s - %s" account.BankAccountType account.IBAN
        IsSelected = state.Invoice.ToAccount = Some account
    }

    let vatRateSelection: FormSelect = {
        Identifier = "vatRate"
        OnChanged = (fun e -> VatRateChanged (float (e.TrimEnd('%'))) |> dispatch)
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

    let fromAccountSelection: FormSelect = {
        Identifier = "fromAccount"
        OnChanged = (fun e -> FromAccountChanged (parseAccount (e, state.CurrentBuilding.BankAccounts)) |> dispatch)
        Options = state.CurrentBuilding.BankAccounts |> List.map fromAccountToOption
    }

    let toAccountSelection: FormSelect = 
        let possibleBankAccounts = 
            match state.SelectedOrganization with
            | Some org ->
                org.MainBankAccount::(org.OtherBankAccounts |> List.map Some)
                |> List.choose id
            | None ->
                []

        {
            Identifier = "toAccount"
            OnChanged = (fun e -> ToAccountChanged (parseAccount (e, possibleBankAccounts)) |> dispatch)
            Options = possibleBankAccounts |> List.map toAccountToOption
        }

    let calculateVat (cost: string option) (vatRate: float) = 
        match cost with
        | Some x ->
            match Double.TryParse(x) with
            | true, parsed -> Math.Round(parsed * vatRate / 100.0, 2)
            | false, _ -> 0.0
        | None ->
            0.0

    let renderFinancialCategoryModal =
        FunctionComponent.Of((fun (props: {| SelectedCategoryCode: string option; AllCategories: FinancialCategory list; Showing: bool |}) ->
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
                                    AllItems = props.AllCategories |> List.sortBy (fun cat -> cat.Code)
                                    SelectedItems = 
                                        match props.SelectedCategoryCode with
                                        | Some selected -> props.AllCategories |> List.filter (fun cat -> cat.Code = selected)
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
                Input [
                    Type "date"
                    OnChange (fun e -> BookingDateChanged e.Value |> dispatch)
                    valueOrDefault (state.Invoice.BookingDate)
                ]
                FormError (errorFor (nameof state.Invoice.BookingDate))
            ]
            |> inColumn

            formGroup [
                Label "Rubriek"

                let shown =
                    match state.Invoice.CategoryCode, state.Invoice.CategoryDescription with
                    | Some catCode, Some catDescription -> sprintf "%s - %s" catCode catDescription
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
                    valueOrDefault state.Invoice.OrganizationName
                ]
                FormError (errorFor (nameof state.Invoice.OrganizationName))
            ]
            |> inColumn

            match state.Invoice.OrganizationVatNumber, state.Invoice.OrganizationNumber with
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
                    valueOrDefault (state.Invoice.ExternalInvoiceNumber)
                ]
                FormError (errorFor (nameof state.Invoice.InvoiceNumber))
            ]
            |> inColumn

            formGroup [
                Label "Factuurdatum"
                Input [
                    Type "date"
                    OnChange (fun e -> InvoiceDateChanged e.Value |> dispatch)
                    valueOrDefault (state.Invoice.InvoiceDate)
                ]
                FormError (errorFor (nameof state.Invoice.InvoiceDate))
            ]
            |> inColumn

            formGroup [
                Label "Uiterste betaaldatum"
                Input [
                    Type "date"
                    OnChange (fun e -> DueDateChanged e.Value |> dispatch)
                    valueOrDefault (state.Invoice.DueDate)
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
                Label "Van rekening"
                Select fromAccountSelection
                FormError (errorFor (nameof state.Invoice.FromAccount))
            ]
            |> inColumn

            formGroup [
                Label "IBAN"
                Input [
                    Type "text"
                    Disabled true
                    valueOrDefault (state.Invoice.FromAccount |> Option.map (fun f -> f.IBAN))
                ]
            ]
            |> inColumn

            formGroup [
                Label "BIC"
                Input [
                    Type "text"
                    Disabled true
                    valueOrDefault (state.Invoice.FromAccount |> Option.map (fun f -> f.BIC))
                ]
            ]
            |> inColumn
        ]

        div [ Class Bootstrap.row ] [
            formGroup [
                Label "Naar rekening"
                match state.Invoice.OrganizationId with
                | Some orgId ->
                    Select toAccountSelection
                | None ->
                    OtherChildren [ div [] [ span [ Class Bootstrap.formControl ] [ str "Gelieve eerst een leverancier te selecteren" ] ] ]
                FormError (errorFor (nameof state.Invoice.ToAccount))
            ]
            |> inColumn

            formGroup [
                Label "IBAN"
                Input [
                    Type "text"
                    MaxLength 64.0
                    Disabled state.Invoice.OrganizationId.IsNone
                    valueOrDefault (state.Invoice.ToAccount |> Option.map (fun f -> f.IBAN))
                ]
                FormError (errorFor (nameof state.Invoice.ToAccount))
            ]
            |> inColumn

            formGroup [
                Label "BIC"
                Input [
                    Type "text"
                    MaxLength 11.0
                    Disabled state.Invoice.OrganizationId.IsNone
                    valueOrDefault (state.Invoice.ToAccount |> Option.map (fun f -> f.BIC))
                ]
                FormError (errorFor (nameof state.Invoice.ToAccount))
            ]
            |> inColumn
        ]

        renderFinancialCategoryModal 
            {| 
                SelectedCategoryCode = state.Invoice.CategoryCode
                AllCategories = state.FinancialCategories 
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
                SelectedOrganizationId = state.Invoice.OrganizationId
                AllOrganizations = state.Organizations 
                Showing = state.ShowingOrganizationModal
            |}
    ]