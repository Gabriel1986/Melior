namespace Client

open Elmish
open Elmish.Debug
open Elmish.React
open Elmish.Navigation
open Fable
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
open Feliz.Router

open Shared.Domain

open Routing
open Client
open Client.Buildings
open Client.ClientStyle
open Client.Residents
open Client.Lots

module Client =
    importAll "./public/styles/bootstrap.min.css"
    importAll "./public/styles/fontawesome.all.min.css"

    type ClientState =
        | Initializing
        | Running of RunningState
        | Stopped of fatalError: exn
    and RunningState = {
        CurrentUser: CurrentUser
        CurrentPage: Page
    }

    type ClientMessage =
        | StartApplication of CurrentUser
        | StopApplication of exn
        | PageNotFound
        | ChangePage of Routing.Page

    let init () =
        let cmd =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi().GetCurrentUser)
                ()
                StartApplication
                StopApplication
        Initializing, cmd

    let update (message: ClientMessage) (state: ClientState) =
        match message with
        | StartApplication currentUser ->
            let currentPage = Routing.parseUrl (Router.currentUrl ())

            Running {
                CurrentUser = currentUser
                CurrentPage = currentPage
            }, Cmd.none
        | StopApplication e ->
            Stopped e, Cmd.none
        | PageNotFound ->
            match state with
            | Running running ->
                Running { running with CurrentPage = NotFound }, Cmd.none
            | _ ->
                state, Cmd.none
        | ChangePage newPage ->
            match state with
            | Running running ->
                Running { running with CurrentPage = newPage }, Cmd.none   
            | _ ->
                state, Cmd.none

    let private shouldChangePage (oldPage: Page) (newPage: Page) =
        match oldPage with
        | Page.BuildingDetails _
        | Page.BuildingList -> 
            match newPage with 
            | Page.BuildingDetails _ 
            | Page.BuildingList -> false
            | _ -> true
        | Page.ResidentDetails _
        | Page.ResidentList ->
            match newPage with
            | Page.ResidentDetails _
            | Page.ResidentList -> false
            | _ -> true
        | Page.LotDetails _
        | Page.LotList ->
            match newPage with
            | Page.LotDetails _
            | Page.LotList -> false
            | _ -> true
        | _ -> true

    let view (state: ClientState) (dispatch: ClientMessage -> unit) =
        match state with
        | Initializing ->
            div [] [                
                Navbar.render {| CurrentPage = None; CurrentUser = None |}
                div [ Class Bootstrap.py3 ] [ h2 [] [ str "De app wordt gestart" ] ] 
            ]
        | Running runningState ->
            let currentPage =
                div [ Class (sprintf "%s %s" Bootstrap.containerFluid Bootstrap.py3) ] [
                    match runningState.CurrentPage with
                    | Page.Portal -> 
                        PortalPage.render {| CurrentUser = runningState.CurrentUser |}
                    | Page.BuildingList ->
                        BuildingsPage.render {| CurrentUser = runningState.CurrentUser; BuildingId = None |}
                    | Page.BuildingDetails buildingId ->
                        BuildingsPage.render {| CurrentUser = runningState.CurrentUser; BuildingId = Some buildingId |}
                    | Page.ResidentList ->
                        ResidentsPage.render {| CurrentUser = runningState.CurrentUser; ResidentId = None |}
                    | Page.ResidentDetails residentId ->
                        ResidentsPage.render {| CurrentUser = runningState.CurrentUser; ResidentId = Some residentId |}
                    | Page.LotList ->
                        LotsPage.render {| CurrentUser = runningState.CurrentUser; LotId = None |}
                    | Page.LotDetails lotId ->
                        LotsPage.render {| CurrentUser = runningState.CurrentUser; LotId = Some lotId |}
                    | Page.NotFound        -> div [] [ p [] [ str "Pagina werd niet gevonden." ] ]
                ]

            div [] [
                Navbar.render {| CurrentPage = Some runningState.CurrentPage; CurrentUser = Some runningState.CurrentUser |}
                Router.router [
                    Router.onUrlChanged (fun urlParts ->
                        let newPage = Routing.parseUrl urlParts
                        if runningState.CurrentPage = newPage then ()
                        elif shouldChangePage runningState.CurrentPage newPage then ChangePage newPage |> dispatch
                        else ()
                    )
                    Router.application currentPage
                ]
            ]
        | Stopped _ ->
            div [] [
                Navbar.render {| CurrentPage = None; CurrentUser = None |}
                str "Er is iets misgelopen bij het laden van de applicatie."
            ]

    Program.mkProgram init update view
    #if DEBUG
    |> Program.withConsoleTrace
    #endif
    |> Program.withReactBatched "elmish-app"
    #if DEBUG
    |> Program.withDebugger
    #endif
    //|> Program.withSubscription poller
    |> Program.run
