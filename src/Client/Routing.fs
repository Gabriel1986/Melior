module Client.Routing
    open System
    open Elmish
    open Feliz.Router

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
        | DistributionKeyList of BuildingSpecificProps
        | DistributionKeyDetails of BuildingSpecificDetailProps
        | CostDiary of BuildingSpecificProps //Invoices
        | InvoiceDetails of BuildingSpecificDetailProps
        | IncomeDiary of BuildingSpecificProps //Management of owner deposits
        | FinancialDiary of BuildingSpecificProps //Bank debit/credit
        | NotFound
        | NoticeBoard
        | MyEvents
        | MyLots
        | MyContracts
        | MyFinancials
    
    let [<Literal>] private BuildingsPage: string = "buildings"
    let [<Literal>] private PortalPage: string = ""
    let [<Literal>] private OwnersPage: string = "owners"
    let [<Literal>] private LotsPage: string = "lots"
    let [<Literal>] private OrganizationsPage: string = "organizations"
    let [<Literal>] private ProfessionalSyndicsPage: string = "professionalSyndics"
    let [<Literal>] private OrganizationTypesPage: string = "organizationTypes"
    let [<Literal>] private ContractsPage: string = "contracts"
    let [<Literal>] private MyContractsPage = "myContracts"
    let [<Literal>] private MyEventsPage = "myEvents"
    let [<Literal>] private MyLotsPage = "myLots"
    let [<Literal>] private MyFinancialsPage = "myFinancials"
    let [<Literal>] private NoticeBoardPage = "noticeboard"
    let [<Literal>] private DistributionKeysPage = "distributionKeys"
    let [<Literal>] private CostDiaryPage = "costDiary"
    let [<Literal>] private IncomeDiaryPage = "incomeDiary"
    let [<Literal>] private FinancialDiaryPage = "financialDiary"

    let private navigateToDetailsPage (identifier: Guid) (page: string) =
        Router.navigate(page, string identifier)

    let private navigateToBuildingSpecificPage (props: BuildingSpecificProps) (page: string) =
        Router.navigate(BuildingsPage, string props.BuildingId, page)

    let private navigateToBuildingSpecificDetailsPage (props: BuildingSpecificDetailProps) (page: string) =
        Router.navigate(BuildingsPage, string props.BuildingId, page, string props.DetailId)

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
        | Page.DistributionKeyList specifics ->
            routeToSpecificPage(DistributionKeysPage, specifics)
        | Page.DistributionKeyDetails specifics ->
            routeToSpecificDetailsPage(DistributionKeysPage, specifics)
        | Page.CostDiary specifics ->
            routeToSpecificPage(CostDiaryPage, specifics)
        | Page.InvoiceDetails specifics ->
            routeToSpecificDetailsPage(CostDiaryPage, specifics)
        | Page.IncomeDiary specifics ->
            routeToSpecificPage(IncomeDiaryPage, specifics)
        | Page.FinancialDiary specifics ->
            routeToSpecificPage(FinancialDiaryPage, specifics)

    let navigateToPage =
        function
        | Page.Portal -> 
            Router.navigate(PortalPage)
        | Page.BuildingList -> 
            Router.navigate(BuildingsPage)
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
            Router.navigate(ProfessionalSyndicsPage)
        | Page.ProfessionalSyndicDetails props ->
            ProfessionalSyndicsPage |> navigateToDetailsPage props
        | Page.OrganizationTypeList ->
            Router.navigate(OrganizationTypesPage)
        | Page.Contracts props ->
            ContractsPage |> navigateToBuildingSpecificPage props
        | Page.MyContracts ->
            Router.navigate(MyContractsPage)
        | Page.MyEvents ->
            Router.navigate(MyEventsPage)
        | Page.MyLots ->
            Router.navigate(MyLotsPage)
        | Page.MyFinancials ->
            Router.navigate(MyFinancialsPage)
        | Page.NoticeBoard ->
            Router.navigate(NoticeBoardPage)
        | Page.DistributionKeyList props ->
            DistributionKeysPage |> navigateToBuildingSpecificPage props
        | Page.DistributionKeyDetails props ->
            DistributionKeysPage |> navigateToBuildingSpecificDetailsPage props
        | Page.CostDiary props ->
            CostDiaryPage |> navigateToBuildingSpecificPage props
        | Page.InvoiceDetails props ->
           CostDiaryPage |> navigateToBuildingSpecificDetailsPage props
        | Page.IncomeDiary props ->
            IncomeDiaryPage |> navigateToBuildingSpecificPage props
        | Page.FinancialDiary props ->
            FinancialDiaryPage |> navigateToBuildingSpecificPage props
        | Page.NotFound ->
            //Do nothing... you're not supposed to go to the loading or notfound page from code...
            Cmd.none

    let parseUrl = function
        | [ ] -> 
            Page.Portal
        | [ BuildingsPage ] -> 
            Page.BuildingList
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId ] -> 
            Page.BuildingDetails buildingId
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId; OwnersPage ] -> 
            Page.OwnerList { BuildingId = buildingId }
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId; OwnersPage ; Feliz.Router.Route.Guid personId ] -> 
            Page.OwnerDetails { BuildingId = buildingId; DetailId = personId }
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId; LotsPage ] -> 
            Page.LotList { BuildingId = buildingId }
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId; LotsPage ; Feliz.Router.Route.Guid lotId ] -> 
            Page.LotDetails { BuildingId = buildingId; DetailId = lotId }
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId; OrganizationsPage ] -> 
            Page.OrganizationList { BuildingId = buildingId }
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId; OrganizationsPage ; Feliz.Router.Route.Guid orgId ] -> 
            Page.OrganizationDetails { BuildingId = buildingId; DetailId = orgId }
        | [ ProfessionalSyndicsPage ] ->
            Page.ProfessionalSyndicList
        | [ ProfessionalSyndicsPage; Feliz.Router.Route.Guid proSyndicId ] ->
            Page.ProfessionalSyndicDetails proSyndicId
        | [ OrganizationTypesPage ] ->
            Page.OrganizationTypeList
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId; ContractsPage ] ->
            Page.Contracts { BuildingId = buildingId }
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId; DistributionKeysPage ] ->
            Page.DistributionKeyList { BuildingId = buildingId }
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId; DistributionKeysPage ; Feliz.Router.Route.Guid distributionKeyId ] -> 
            Page.LotDetails { BuildingId = buildingId; DetailId = distributionKeyId }
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId; CostDiaryPage ] ->
            Page.CostDiary { BuildingId = buildingId }
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId; CostDiaryPage ; Feliz.Router.Route.Guid invoiceId ] ->
            Page.InvoiceDetails { BuildingId = buildingId; DetailId = invoiceId }
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId; IncomeDiaryPage ] ->
            Page.IncomeDiary { BuildingId = buildingId }
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId; FinancialDiaryPage ] ->
            Page.FinancialDiary { BuildingId = buildingId }
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
        | _ -> 
            Page.NotFound