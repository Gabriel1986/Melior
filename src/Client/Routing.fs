module Client.Routing
    open System
    open Elmish

    type Page =
        | Portal
        | BuildingList
        | BuildingDetails of Guid
        | ResidentList
        | ResidentDetails of Guid
        | LotList
        | LotDetails of Guid
        | NotFound

    type NavigablePage =
        | Portal
        | BuildingList
        | ResidentList
        | LotList
    
    let [<Literal>] private BuildingsPage: string = "buildings"
    let [<Literal>] private PortalPage: string = ""
    let [<Literal>] private ResidentsPage: string = "residents"
    let [<Literal>] private LotsPage: string = "lots"

    let navigateToPage =
        function
        | Page.Portal -> 
            Feliz.Router.Router.navigate(PortalPage)
        | Page.BuildingList -> 
            Feliz.Router.Router.navigate(BuildingsPage)
        | Page.BuildingDetails buildingId -> 
            Feliz.Router.Router.navigate(BuildingsPage, string buildingId)
        | Page.ResidentList ->
            Feliz.Router.Router.navigate(ResidentsPage)
        | Page.ResidentDetails residentId ->
            Feliz.Router.Router.navigate(ResidentsPage, string residentId)
        | Page.LotList ->
            Feliz.Router.Router.navigate(LotsPage)
        | Page.LotDetails lotId ->
            Feliz.Router.Router.navigate(LotsPage, string lotId)
        | Page.NotFound ->
            //Do nothing... you're not supposed to go to the loading or notfound page from code...
            Cmd.none

    let navigateToNavigablePage =
        function
        | NavigablePage.Portal -> navigateToPage Page.Portal
        | NavigablePage.BuildingList -> navigateToPage Page.BuildingList
        | NavigablePage.ResidentList -> navigateToPage Page.ResidentList
        | NavigablePage.LotList -> navigateToPage Page.LotList

    let parseUrl = function
        | [ ]                                                    -> Page.Portal
        | [ BuildingsPage ]                                      -> Page.BuildingList
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId ] -> Page.BuildingDetails buildingId
        | [ ResidentsPage ]                                      -> Page.ResidentList
        | [ ResidentsPage ; Feliz.Router.Route.Guid residentId ] -> Page.ResidentDetails residentId
        | [ LotsPage ]                                           -> Page.LotList
        | [ LotsPage ; Feliz.Router.Route.Guid lotId ]           -> Page.LotDetails lotId
        | _                                                      -> Page.NotFound