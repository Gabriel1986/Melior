module Client.Routing
    open System
    open Elmish

    type Page =
        | Portal
        | BuildingList
        | BuildingDetails of Guid
        | NotFound

    type NavigablePage =
        | Portal
        | BuildingList
    
    let [<Literal>] private BuildingsPage: string = "buildings"
    let [<Literal>] private PortalPage: string = ""

    let navigateToPage =
        function
        | Page.Portal -> 
            printf "Navigating to: Portal page"
            Feliz.Router.Router.navigate(PortalPage)
        | Page.BuildingList -> 
            printf "Navigating to: buildings list"
            Feliz.Router.Router.navigate(BuildingsPage)
        | Page.BuildingDetails buildingId -> 
            printf "Navigating to: building details"
            Feliz.Router.Router.navigate(BuildingsPage, string buildingId)
        | Page.NotFound ->
            //Do nothing... you're not supposed to go to the loading or notfound page from code...
            Cmd.none

    let navigateToNavigablePage =
        function
        | NavigablePage.Portal -> navigateToPage Page.Portal
        | NavigablePage.BuildingList -> navigateToPage Page.BuildingList

    let parseUrl = function
        | [ ]                                                    -> Page.Portal
        | [ BuildingsPage ]                                      -> Page.BuildingList
        | [ BuildingsPage ; Feliz.Router.Route.Guid buildingId ] -> Page.BuildingDetails buildingId
        | _                                                      -> Page.NotFound