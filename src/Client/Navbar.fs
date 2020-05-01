module Client.Navbar

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Domain
open Routing
open Client
open Client.ClientStyle

type NavbarState = {
    CurrentUser: CurrentUser option
    CurrentPage: Page option
}

type NavbarMsg =
    | NavigateToPage of page: NavigablePage

let init (props: {| CurrentUser: CurrentUser option; CurrentPage: Page option |}) =
    {
        CurrentUser = props.CurrentUser
        CurrentPage = props.CurrentPage
    }, Cmd.none

let update (msg: NavbarMsg) (currentState: NavbarState): NavbarState * Cmd<NavbarMsg> =
    match msg with
    | NavigateToPage page ->
        currentState, Routing.navigateToNavigablePage page

let view (state: NavbarState) (dispatch: NavbarMsg -> unit) =
    let currentlySelectedNavigablePage =
        match state.CurrentPage with
        | Some (Page.Portal) -> Some NavigablePage.Portal
        | Some (Page.BuildingList)
        | Some (Page.BuildingDetails _) -> Some NavigablePage.BuildingList
        | _ -> None

    let determineStyle navigablePage =
        let extraClasses =
            match currentlySelectedNavigablePage with
            | Some currentNavigablePage when navigablePage = currentNavigablePage -> 
                [ MeliorStyle.active ]
            | _ -> 
                []
        String.Join(" ", MeliorStyle.navLink::extraClasses)

    nav [ Class (sprintf "%s %s %s %s" MeliorStyle.navbar MeliorStyle.navbarExpandLg MeliorStyle.navbarDark MeliorStyle.bgDark) ] [ 
        a [ Class MeliorStyle.navbarBrand; Href "#" ] [ str "Top secret project" ]
        ul [ Class MeliorStyle.navbarNav ] [ 
            li [ Class MeliorStyle.navItem ] [ 
                a [ 
                    Class (determineStyle NavigablePage.Portal)
                    OnClick (fun _ -> NavigateToPage NavigablePage.Portal |> dispatch) 
                ] [ str "Portaal" ]
            ]
            li [ Class MeliorStyle.navItem ] [
                a [ 
                    Class (determineStyle NavigablePage.BuildingList)
                    OnClick (fun _ -> NavigateToPage NavigablePage.BuildingList |> dispatch) 
                ] [ str "Gebouwen" ]
            ]
        ]
    ]

let render (props: {| CurrentUser: CurrentUser option; CurrentPage: Page option |}) =
    React.elmishComponent ("BuildingsPage", init props, update, view)