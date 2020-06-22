module Client.Routing
    open System
    open Elmish

    type BuildingSpecificListProps = {
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
        | OwnerList of BuildingSpecificListProps
        | OwnerDetails of BuildingSpecificDetailProps
        | LotList of BuildingSpecificListProps
        | LotDetails of BuildingSpecificDetailProps
        | OrganizationList of BuildingSpecificListProps
        | OrganizationDetails of BuildingSpecificDetailProps
        | ProfessionalSyndicList
        | ProfessionalSyndicDetails of Guid
        | OrganizationTypeList
        | NotFound

    type NavigablePage =
        | Portal
        | BuildingList
        | OwnerList of BuildingSpecificListProps
        | LotList of BuildingSpecificListProps
        | OrganizationList of BuildingSpecificListProps
        | ProfessionalSyndicsList
        | OrganizationTypesList
    
    let [<Literal>] private BuildingsPage: string = "buildings"
    let [<Literal>] private PortalPage: string = ""
    let [<Literal>] private OwnersPage: string = "owners"
    let [<Literal>] private LotsPage: string = "lots"
    let [<Literal>] private OrganizationsPage: string = "organizations"
    let [<Literal>] private ProfessionalSyndicsPage: string = "professionalSyndics"
    let [<Literal>] private OrganizationTypesPage: string = "organizationTypes"

    let private navigateToDetailsPage (identifier: Guid) (page: string) =
        Feliz.Router.Router.navigate(page, string identifier)

    let private navigateToBuildingSpecificListPage (props: BuildingSpecificListProps) (page: string) =
        Feliz.Router.Router.navigate(BuildingsPage, string props.BuildingId, page)

    let private navigateToBuildingSpecificDetailsPage (props: BuildingSpecificDetailProps) (page: string) =
        Feliz.Router.Router.navigate(BuildingsPage, string props.BuildingId, page, string props.DetailId)

    let navigateToPage =
        function
        | Page.Portal -> 
            Feliz.Router.Router.navigate(PortalPage)
        | Page.BuildingList -> 
            Feliz.Router.Router.navigate(BuildingsPage)
        | Page.BuildingDetails props ->
            BuildingsPage |> navigateToDetailsPage props
        | Page.OwnerList props ->
            OwnersPage |> navigateToBuildingSpecificListPage props 
        | Page.OwnerDetails props ->
            OwnersPage |> navigateToBuildingSpecificDetailsPage props
        | Page.LotList props ->
            LotsPage |> navigateToBuildingSpecificListPage props
        | Page.LotDetails props ->
            LotsPage |> navigateToBuildingSpecificDetailsPage props
        | Page.OrganizationList props ->
            OrganizationsPage |> navigateToBuildingSpecificListPage props
        | Page.OrganizationDetails props ->
            OrganizationsPage |> navigateToBuildingSpecificDetailsPage props
        | Page.ProfessionalSyndicList ->
            Feliz.Router.Router.navigate(ProfessionalSyndicsPage)
        | Page.ProfessionalSyndicDetails props ->
            ProfessionalSyndicsPage |> navigateToDetailsPage props
        | Page.OrganizationTypeList ->
            Feliz.Router.Router.navigate(OrganizationTypesPage)
        | Page.NotFound ->
            //Do nothing... you're not supposed to go to the loading or notfound page from code...
            Cmd.none

    let navigateToNavigablePage =
        function
        | NavigablePage.Portal -> navigateToPage Page.Portal
        | NavigablePage.BuildingList -> navigateToPage Page.BuildingList
        | NavigablePage.OwnerList props -> navigateToPage (Page.OwnerList props)
        | NavigablePage.LotList props  -> navigateToPage (Page.LotList props)
        | NavigablePage.OrganizationList props -> navigateToPage (Page.OrganizationList props)
        | NavigablePage.ProfessionalSyndicsList -> navigateToPage Page.ProfessionalSyndicList
        | NavigablePage.OrganizationTypesList -> navigateToPage Page.OrganizationTypeList

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
        | _ -> 
            Page.NotFound