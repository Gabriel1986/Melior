module Client.Routing
    open System
    open Elmish
    open Feliz.Router
    open Fable.React
    open Fable.React.Props

    type BuildingSpecificProps = {
        BuildingId: Guid
    }

    type BuildingSpecificDetailProps = {
        BuildingId: Guid
        DetailId: Guid
    }

    type Page =
        | Portal
        | BuildingList
        | BuildingDetails of Guid
        | OwnerList of BuildingSpecificProps
        | OwnerDetails of BuildingSpecificDetailProps
        | LotList of BuildingSpecificProps
        | LotDetails of BuildingSpecificDetailProps
        | OrganizationList of BuildingSpecificProps
        | OrganizationDetails of BuildingSpecificDetailProps
        | Contracts of BuildingSpecificProps
        | ProfessionalSyndicList
        | ProfessionalSyndicDetails of Guid
        | OrganizationTypeList
        | FinancialSettings of BuildingSpecificProps
        | DistributionKeyList of BuildingSpecificProps
        | DistributionKeyDetails of BuildingSpecificDetailProps
        | Invoices of BuildingSpecificProps //Invoices
        | InvoiceDetails of BuildingSpecificDetailProps
        | DepositRequests of BuildingSpecificProps //Deposits
        | DepositRequestDetails of BuildingSpecificDetailProps
        | BankNotes of BuildingSpecificProps //Bank debit/credit
        | FinancialTransactions of BuildingSpecificProps //Financial transaction overview
        | Balance of BuildingSpecificProps
        | UserList
        | UserDetails of Guid
        | NotFound
        | NoticeBoard
        | MyEvents
        | MyLots
        | MyContracts
        | MyFinancials
    
    let [<Literal>] private BuildingsPage = "buildings"
    let [<Literal>] private PortalPage = ""
    let [<Literal>] private OwnersPage = "owners"
    let [<Literal>] private LotsPage = "lots"
    let [<Literal>] private OrganizationsPage = "organizations"
    let [<Literal>] private ProfessionalSyndicsPage = "professionalSyndics"
    let [<Literal>] private OrganizationTypesPage = "organizationTypes"
    let [<Literal>] private ContractsPage = "contracts"
    let [<Literal>] private MyContractsPage = "myContracts"
    let [<Literal>] private MyEventsPage = "myEvents"
    let [<Literal>] private MyLotsPage = "myLots"
    let [<Literal>] private MyFinancialsPage = "myFinancials"
    let [<Literal>] private NoticeBoardPage = "noticeboard"
    let [<Literal>] private DistributionKeysPage = "distributionKeys"
    let [<Literal>] private FinancialSettingsPage = "financialsettings"
    let [<Literal>] private InvoicesPage = "invoices"
    let [<Literal>] private OwnerDepositsPage = "ownerdeposits"
    let [<Literal>] private BankNotesPage = "banknotes"
    let [<Literal>] private FinancialTransactionsPage = "financialtransactions"
    let [<Literal>] private BalancePage = "balance"
    let [<Literal>] private UsersPage = "users"

    let private navigateToDetailsPage (identifier: Guid) (page: string) =
        Cmd.navigate(page, string identifier)

    let private navigateToBuildingSpecificPage (props: BuildingSpecificProps) (page: string) =
        Cmd.navigate(BuildingsPage, string props.BuildingId, page)

    let private navigateToBuildingSpecificDetailsPage (props: BuildingSpecificDetailProps) (page: string) =
        Cmd.navigate(BuildingsPage, string props.BuildingId, page, string props.DetailId)

    let private routeToSpecificDetailsPage (page: string, specifics: BuildingSpecificDetailProps) =
        Router.format(BuildingsPage, string specifics.BuildingId, page, string specifics.DetailId)

    let private routeToSpecificPage (page: string, specifics: BuildingSpecificProps) =
        Router.format(BuildingsPage, string specifics.BuildingId, page)

    let generateUrl =
        function
        | Page.Portal -> 
            Router.format([])
        | Page.BuildingList -> 
            Router.format(BuildingsPage)
        | Page.BuildingDetails (buildingId) ->
            Router.format(BuildingsPage, string buildingId)
        | Page.OwnerDetails specifics ->
            routeToSpecificDetailsPage (OwnersPage, specifics)
        | Page.OwnerList specifics ->
            routeToSpecificPage (OwnersPage, specifics)
        | Page.LotDetails specifics ->
            routeToSpecificDetailsPage (LotsPage, specifics)
        | Page.LotList specifics ->
            routeToSpecificPage (LotsPage, specifics)
        | Page.OrganizationDetails specifics ->
            routeToSpecificDetailsPage (OrganizationsPage, specifics)
        | Page.OrganizationList specifics ->
            routeToSpecificPage (OrganizationsPage, specifics)
        | Page.ProfessionalSyndicDetails proSyndicId ->
            Router.format(ProfessionalSyndicsPage, string proSyndicId)
        | Page.ProfessionalSyndicList ->
            Router.format(ProfessionalSyndicsPage)
        | Page.OrganizationTypeList ->
            Router.format(OrganizationTypesPage)
        | Page.Contracts specifics ->
            routeToSpecificPage(ContractsPage, specifics)
        | Page.MyContracts ->
            Router.format(MyContractsPage)
        | Page.MyEvents ->
            Router.format(MyEventsPage)
        | Page.MyLots ->
            Router.format(MyLotsPage)
        | Page.MyFinancials ->
            Router.format(MyFinancialsPage)
        | Page.NoticeBoard ->
            Router.format(NoticeBoardPage)
        | Page.NotFound ->
            Router.format(PortalPage)
        | Page.FinancialSettings specifics ->
            routeToSpecificPage(FinancialSettingsPage, specifics)
        | Page.DistributionKeyList specifics ->
            routeToSpecificPage(DistributionKeysPage, specifics)
        | Page.DistributionKeyDetails specifics ->
            routeToSpecificDetailsPage(DistributionKeysPage, specifics)
        | Page.Invoices specifics ->
            routeToSpecificPage(InvoicesPage, specifics)
        | Page.InvoiceDetails specifics ->
            routeToSpecificDetailsPage(InvoicesPage, specifics)
        | Page.DepositRequests specifics ->
            routeToSpecificPage(OwnerDepositsPage, specifics)
        | Page.DepositRequestDetails specifics ->
            routeToSpecificDetailsPage(OwnerDepositsPage, specifics)
        | Page.BankNotes specifics ->
            routeToSpecificPage(BankNotesPage, specifics)
        | Page.FinancialTransactions specifics ->
            routeToSpecificPage(FinancialTransactionsPage, specifics)
        | Page.Balance specifics ->
            routeToSpecificPage(BalancePage, specifics)
        | Page.UserList ->
            Router.format(UsersPage)
        | Page.UserDetails userId ->
            Router.format(UsersPage, string userId)

    let navigateToPage =
        function
        | Page.Portal -> 
            Cmd.navigate(PortalPage)
        | Page.BuildingList -> 
            Cmd.navigate(BuildingsPage)
        | Page.BuildingDetails props ->
            BuildingsPage |> navigateToDetailsPage props
        | Page.OwnerList props ->
            OwnersPage |> navigateToBuildingSpecificPage props 
        | Page.OwnerDetails props ->
            OwnersPage |> navigateToBuildingSpecificDetailsPage props
        | Page.LotList props ->
            LotsPage |> navigateToBuildingSpecificPage props
        | Page.LotDetails props ->
            LotsPage |> navigateToBuildingSpecificDetailsPage props
        | Page.OrganizationList props ->
            OrganizationsPage |> navigateToBuildingSpecificPage props
        | Page.OrganizationDetails props ->
            OrganizationsPage |> navigateToBuildingSpecificDetailsPage props
        | Page.ProfessionalSyndicList ->
            Cmd.navigate(ProfessionalSyndicsPage)
        | Page.ProfessionalSyndicDetails props ->
            ProfessionalSyndicsPage |> navigateToDetailsPage props
        | Page.OrganizationTypeList ->
            Cmd.navigate(OrganizationTypesPage)
        | Page.Contracts props ->
            ContractsPage |> navigateToBuildingSpecificPage props
        | Page.MyContracts ->
            Cmd.navigate(MyContractsPage)
        | Page.MyEvents ->
            Cmd.navigate(MyEventsPage)
        | Page.MyLots ->
            Cmd.navigate(MyLotsPage)
        | Page.MyFinancials ->
            Cmd.navigate(MyFinancialsPage)
        | Page.NoticeBoard ->
            Cmd.navigate(NoticeBoardPage)
        | Page.FinancialSettings props ->
            FinancialSettingsPage |> navigateToBuildingSpecificPage props
        | Page.DistributionKeyList props ->
            DistributionKeysPage |> navigateToBuildingSpecificPage props
        | Page.DistributionKeyDetails props ->
            DistributionKeysPage |> navigateToBuildingSpecificDetailsPage props
        | Page.Invoices props ->
            InvoicesPage |> navigateToBuildingSpecificPage props
        | Page.InvoiceDetails props ->
            InvoicesPage |> navigateToBuildingSpecificDetailsPage props
        | Page.DepositRequests props ->
            OwnerDepositsPage |> navigateToBuildingSpecificPage props
        | Page.DepositRequestDetails props ->
            OwnerDepositsPage |> navigateToBuildingSpecificDetailsPage props
        | Page.BankNotes props ->
            BankNotesPage |> navigateToBuildingSpecificPage props
        | Page.FinancialTransactions props ->
            FinancialTransactionsPage |> navigateToBuildingSpecificPage props
        | Page.Balance props ->
            BalancePage |> navigateToBuildingSpecificPage props
        | Page.UserList ->
            Cmd.navigate(UsersPage)
        | Page.UserDetails props ->
            UsersPage |> navigateToDetailsPage props
        | Page.NotFound ->
            //Do nothing... you're not supposed to go to the loading or notfound page from code...
            Cmd.none

    let parseUrl = function
        | [ ] -> 
            Page.Portal
        | [ BuildingsPage ] -> 
            Page.BuildingList
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId ] -> 
            Page.BuildingDetails buildingId
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; OwnersPage ] -> 
            Page.OwnerList { BuildingId = buildingId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; OwnersPage; Feliz.Router.Route.Guid personId ] -> 
            Page.OwnerDetails { BuildingId = buildingId; DetailId = personId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; LotsPage ] -> 
            Page.LotList { BuildingId = buildingId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; LotsPage; Feliz.Router.Route.Guid lotId ] -> 
            Page.LotDetails { BuildingId = buildingId; DetailId = lotId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; OrganizationsPage ] -> 
            Page.OrganizationList { BuildingId = buildingId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; OrganizationsPage; Feliz.Router.Route.Guid orgId ] -> 
            Page.OrganizationDetails { BuildingId = buildingId; DetailId = orgId }
        | [ ProfessionalSyndicsPage ] ->
            Page.ProfessionalSyndicList
        | [ ProfessionalSyndicsPage; Feliz.Router.Route.Guid proSyndicId ] ->
            Page.ProfessionalSyndicDetails proSyndicId
        | [ OrganizationTypesPage ] ->
            Page.OrganizationTypeList
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; ContractsPage ] ->
            Page.Contracts { BuildingId = buildingId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; FinancialSettingsPage ] ->
            Page.FinancialSettings { BuildingId = buildingId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; DistributionKeysPage ] ->
            Page.DistributionKeyList { BuildingId = buildingId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; DistributionKeysPage; Feliz.Router.Route.Guid distributionKeyId ] -> 
            Page.DistributionKeyDetails { BuildingId = buildingId; DetailId = distributionKeyId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; InvoicesPage ] ->
            Page.Invoices { BuildingId = buildingId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; InvoicesPage; Feliz.Router.Route.Guid invoiceId ] ->
            Page.InvoiceDetails { BuildingId = buildingId; DetailId = invoiceId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; OwnerDepositsPage ] ->
            Page.DepositRequests { BuildingId = buildingId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; OwnerDepositsPage; Feliz.Router.Route.Guid requestId ] ->
            Page.DepositRequestDetails { BuildingId = buildingId; DetailId = requestId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; BankNotesPage ] ->
            Page.BankNotes { BuildingId = buildingId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; FinancialTransactionsPage ] ->
            Page.FinancialTransactions { BuildingId = buildingId }
        | [ BuildingsPage; Feliz.Router.Route.Guid buildingId; BalancePage ] ->
            Page.Balance { BuildingId = buildingId }
        | [ MyContractsPage ] ->
            Page.MyContracts
        | [ MyEventsPage ] ->
            Page.MyEvents
        | [ MyLotsPage ] ->
            Page.MyLots
        | [ MyFinancialsPage ] ->
            Page.MyFinancials
        | [ NoticeBoardPage ] ->
            Page.NoticeBoard
        | [ UsersPage ] ->
            Page.UserList
        | [ UsersPage; Feliz.Router.Route.Guid userId ] ->
            Page.UserDetails userId
        | _ -> 
            Page.NotFound

    let wrapInLink (page: Page) (element: ReactElement) =
        let url = generateUrl page

        let onClick (e: Browser.Types.MouseEvent) =
            e.preventDefault();
            e.stopPropagation();
            Browser.Dom.window.location.href <- url

        a [ Href url; OnClick onClick ] [
            element
        ]