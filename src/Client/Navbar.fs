module Client.Navbar

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Read
open Routing
open Client
open Client.ClientStyle
open Client.ClientStyle.Helpers

type NavbarState = {
    CurrentUser: User option
    CurrentPage: Page option
    CurrentBuilding: BuildingListItem option
}

type NavbarMsg =
    | NavigateToPage of page: Page

let init (props: {| CurrentUser: User option; CurrentPage: Page option; CurrentBuilding: BuildingListItem option |}) =
    {
        CurrentUser = props.CurrentUser
        CurrentPage = props.CurrentPage
        CurrentBuilding = props.CurrentBuilding
    }, Cmd.none

let update (msg: NavbarMsg) (currentState: NavbarState): NavbarState * Cmd<NavbarMsg> =
    match msg with
    | NavigateToPage page ->
        currentState, Routing.navigateToPage page

let determineStyle selectedPage page =
    let extraClasses =
        match selectedPage with
        | Some currentNavigablePage when page = currentNavigablePage -> 
            [ Bootstrap.active ]
        | _ -> 
            []
    String.Join(" ", Bootstrap.navLink::extraClasses)

let renderBuildingSpecificNavigation (currentPage: Page option) (building: BuildingListItem) (dispatch: NavbarMsg -> unit) = 
    let buildingSpecificProps = { BuildingId = building.BuildingId }
    [
        li [ classes [ Bootstrap.navbarText; Bootstrap.plSm5 ] ] [
            span [] [ str (sprintf "%s: "building.Code) ]
        ]

        li [ Class Bootstrap.navItem ] [
            a [
                Class (determineStyle currentPage (Page.OwnerList buildingSpecificProps))
                OnClick (fun _ -> NavigateToPage (Page.OwnerList buildingSpecificProps) |> dispatch)
            ] [ str "Eigenaars" ]
        ]
        li [ Class Bootstrap.navItem ] [
            a [
                Class (determineStyle currentPage (Page.LotList buildingSpecificProps))
                OnClick (fun _ -> NavigateToPage (Page.LotList buildingSpecificProps) |> dispatch)
            ] [ str "Kavels" ]
        ]
        li [ Class Bootstrap.navItem ] [
            a [
                Class (determineStyle currentPage (Page.OrganizationList buildingSpecificProps))
                OnClick (fun _ -> NavigateToPage (Page.OrganizationList buildingSpecificProps) |> dispatch)
            ] [ str "Organisaties" ]
        ]
        li [ Class Bootstrap.navItem ] [
            a [
                Class (determineStyle currentPage (Page.Contracts buildingSpecificProps))
                OnClick (fun _ -> NavigateToPage (Page.Contracts buildingSpecificProps) |> dispatch)                
            ] [ str "Contracten" ]
        ]
    ]

let view (state: NavbarState) (dispatch: NavbarMsg -> unit) =
    let currentPage =
        match state.CurrentPage with
        | Some (Page.Portal)                      -> Some (Page.Portal)
        | Some (Page.BuildingList)                
        | Some (Page.BuildingDetails _)           -> Some (Page.BuildingList)
        | Some (Page.OwnerList p)                 -> Some (Page.OwnerList p)        
        | Some (Page.OwnerDetails p)              -> Some (Page.OwnerList { BuildingId = p.BuildingId })
        | Some (Page.LotList p)                   -> Some (Page.LotList p)
        | Some (Page.LotDetails p)                -> Some (Page.LotList { BuildingId = p.BuildingId })
        | Some (Page.OrganizationList p)          -> Some (Page.OrganizationList p)
        | Some (Page.OrganizationDetails p)       -> Some (Page.OrganizationList { BuildingId = p.BuildingId })
        | Some (Page.ProfessionalSyndicList)
        | Some (Page.ProfessionalSyndicDetails _) -> Some (Page.ProfessionalSyndicList)
        | Some (Page.OrganizationTypeList)        -> Some (Page.OrganizationTypeList)
        | Some (Page.Contracts p)                 -> Some (Page.Contracts p)
        | Some (Page.NotFound)
        | None                                    -> None

    nav [ classes [ Bootstrap.navbar; Bootstrap.navbarExpandSm; Bootstrap.navbarDark; Bootstrap.bgDark ] ] [ 
        a [ Class Bootstrap.navbarBrand; Href "#" ] [ str "Top secret project" ]
        ul [ Class Bootstrap.navbarNav ] [ 
            yield
                li [ Class Bootstrap.navItem ] [ 
                    a [ 
                        Class (determineStyle currentPage Page.Portal)
                        OnClick (fun _ -> NavigateToPage Page.Portal |> dispatch) 
                    ] [ str "Portaal" ]
                ]
            yield
                li [ Class Bootstrap.navItem ] [
                    a [ 
                        Class (determineStyle currentPage Page.BuildingList)
                        OnClick (fun _ -> NavigateToPage Page.BuildingList |> dispatch) 
                    ] [ str "Gebouwen" ]
                ]
            yield
                li [ Class Bootstrap.navItem ] [
                    a [
                        Class (determineStyle currentPage Page.ProfessionalSyndicList)
                        OnClick (fun _ -> NavigateToPage Page.ProfessionalSyndicList |> dispatch)
                    ] [ str "Pro syndici (tijdelijke link)" ]
                ]
            yield
                li [ Class Bootstrap.navItem ] [
                    a [
                        Class (determineStyle currentPage Page.OrganizationTypeList)
                        OnClick (fun _ -> NavigateToPage Page.OrganizationTypeList |> dispatch)
                    ] [ str "Organisatietypes (tijdelijke link)" ]                
                ]
            yield!
                match state.CurrentBuilding with
                | Some building -> renderBuildingSpecificNavigation currentPage building dispatch
                | None -> []
        ]
    ]

let render (props: {| CurrentUser: User option; CurrentPage: Page option; CurrentBuilding: BuildingListItem option |}) =
    React.elmishComponent ("BuildingsPage", init props, update, view)