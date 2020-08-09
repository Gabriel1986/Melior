﻿namespace Client

open System
open Elmish
open Elmish.Debug
open Elmish.React
open Elmish.Navigation
open Fable
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
open Feliz.Router

open Shared.Read

open Routing
open Client
open Client.Buildings
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Owners
open Client.Organizations
open Client.Lots
open Client.ProfessionalSyndics
open Client.Contracts

module Client =
    importAll "./public/styles/bootstrap.min.css"
    importAll "./public/styles/fontawesome.all.min.css"
    importAll "./public/styles/flatpickr.css"
    importAll "./public/styles/b4vtabs.min.css"

    type ClientState =
        | Initializing
        | Running of RunningState
        | Stopped of fatalError: exn
    and RunningState = {
        CurrentUser: User
        CurrentBuilding: BuildingListItem option
        CurrentPage: Page
        CurrentState: PageState option
    }
    and PageState =
        | BuildingPageState of BuildingsPage.State
    type ClientMessage =
        | CurrentUserFetched of User
        | StartApplication of User * BuildingListItem option
        | StopApplication of exn
        | PageNotFound
        | ChangePage of Routing.Page
        | CurrentBuildingChanged of BuildingListItem
        | ChangeCurrentBuildingAndPage of BuildingListItem * Routing.Page
        | BuildingPageMsg of BuildingsPage.Msg

    let init () =
        let cmd =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi().GetCurrentUser) ()
                CurrentUserFetched
                StopApplication
        Initializing, cmd

    let private fetchBuildingAndChangePage buildingId page =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetBuilding) 
            buildingId
            (fun result -> 
                match result with 
                | Some building -> ChangeCurrentBuildingAndPage (building.ToListItem(), page)
                | None -> ChangePage NotFound)
            StopApplication

    let private setCurrentBuilding (running: RunningState) (newBuilding: BuildingListItem) =
        let currentBuildingStr = Thoth.Json.Encode.Auto.toString(0, newBuilding)
        Browser.WebStorage.localStorage.setItem("currentBuilding", currentBuildingStr)
        { running with CurrentBuilding = Some newBuilding }

    let private initializeBuildingsPage props =
        let s, cmd = BuildingsPage.init props
        s |> BuildingPageState |> Some, cmd |> Cmd.map BuildingPageMsg |> Some

    let update (message: ClientMessage) (state: ClientState) =
        let getFirstBuildingId =            
            function 
            | UserRole buildingIds -> buildingIds |> List.tryHead 
            | SyndicRole buildingIds -> buildingIds |> List.tryHead 
            | ProfessionalSyndicRole (_, buildingIds) -> buildingIds |> List.tryHead
            | SysAdminRole -> None

        match message with
        | CurrentUserFetched currentUser ->
            let newCmd = 
                match Library.getCachedCurrentBuilding currentUser with
                | Some currentBuilding -> 
                    Cmd.ofMsg (StartApplication (currentUser, Some currentBuilding))
                | None ->
                    match currentUser.Roles with
                    | [] ->
                        Cmd.ofMsg (StopApplication (exn "U heeft geen rollen binnen de applicatie..."))
                    | roles ->
                        match roles |> List.tryPick getFirstBuildingId with
                        | Some buildingId -> 
                            Cmd.OfAsync.either 
                                (Client.Remoting.getRemotingApi().GetBuilding) buildingId 
                                (fun building ->
                                    building
                                    |> Option.map (fun b -> b.ToListItem())
                                    |> (fun currentBuilding -> currentUser, currentBuilding)
                                    |> StartApplication)
                                StopApplication
                        | None ->
                            //Assume the user is a sysadmin -> pick the first building
                            Cmd.OfAsync.either
                                (Client.Remoting.getRemotingApi().GetBuildings) ()
                                (List.tryHead
                                    >> (fun currentBuilding -> currentUser, currentBuilding)
                                    >> StartApplication)
                                StopApplication
            state, newCmd
        | StartApplication (currentUser, currentBuilding) ->
            let currentPage = Routing.parseUrl (Router.currentUrl ())
            let currentPageState, currentPageCmd =
                match currentPage with
                | Page.BuildingList ->
                    initializeBuildingsPage 
                        {| 
                            CurrentUser = currentUser
                            BuildingId = None
                            CurrentBuildingId = currentBuilding |> Option.map (fun b -> b.BuildingId)
                        |}
                | Page.BuildingDetails buildingId ->
                    initializeBuildingsPage
                        {| 
                            CurrentUser = currentUser
                            BuildingId = Some buildingId
                            CurrentBuildingId = currentBuilding |> Option.map (fun b -> b.BuildingId)
                        |}
                | _ ->
                    None, None


            Running {
                CurrentUser = currentUser
                CurrentPage = currentPage
                CurrentBuilding = currentBuilding
                CurrentState = currentPageState
            }, match currentPageCmd with | Some cmd -> cmd | None -> Cmd.none
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
                let currentBuildingId = running.CurrentBuilding |> Option.map (fun b -> b.BuildingId) |> Option.defaultValue (Guid.NewGuid())                
                match newPage with
                | Page.LotList props
                | Page.OwnerList props
                | Page.OrganizationList props when currentBuildingId <> props.BuildingId ->
                    state, fetchBuildingAndChangePage props.BuildingId newPage
                | Page.LotDetails props
                | Page.OwnerDetails props
                | Page.OrganizationDetails props when currentBuildingId <> props.BuildingId ->
                    //Fetch building and set it as current building, then go to page.
                    state, fetchBuildingAndChangePage props.BuildingId newPage
                | Page.BuildingDetails buildingId ->
                    let currentPageState, currentPageCmd =
                        initializeBuildingsPage 
                            {| 
                                CurrentUser = running.CurrentUser
                                BuildingId = Some buildingId
                                CurrentBuildingId = running.CurrentBuilding |> Option.map (fun b -> b.BuildingId)
                            |}
                    Running { running with CurrentPage = newPage; CurrentState = currentPageState }, currentPageCmd |> Option.defaultValue Cmd.none
                | Page.BuildingList ->
                    let currentPageState, currentPageCmd =
                        initializeBuildingsPage 
                            {| 
                                CurrentUser = running.CurrentUser
                                BuildingId = None
                                CurrentBuildingId = running.CurrentBuilding |> Option.map (fun b -> b.BuildingId)
                            |}
                    Running { running with CurrentPage = newPage; CurrentState = currentPageState }, currentPageCmd |> Option.defaultValue Cmd.none
                | newPage ->
                    Running { running with CurrentPage = newPage }, Cmd.none
            | _ ->
                state, Cmd.none
        | CurrentBuildingChanged newBuilding ->
            match state with
            | Running running ->
                match running.CurrentBuilding with
                | Some currentBuilding when currentBuilding.BuildingId = newBuilding.BuildingId ->
                    state, Cmd.none
                | _ ->
                    let newState = setCurrentBuilding running newBuilding
                    Running newState, Cmd.none
            | _ ->
                state, Cmd.none
        | ChangeCurrentBuildingAndPage (newBuilding, newPage) ->
            match state with
            | Running running ->
                let newRunningState = setCurrentBuilding running newBuilding
                Running { newRunningState with CurrentPage = newPage }, Cmd.none
            | _ ->
                state, Cmd.none
        | BuildingPageMsg msg ->
            match state with
            | Running running ->
                let newState, newCmd =
                    match running.CurrentState with
                    | Some (BuildingPageState s) ->
                        let updatedPageState, pageCommand = BuildingsPage.update msg s

                        let updatedRunningState =
                            match msg with
                            | BuildingsPage.Msg.CurrentBuildingChanged newCurrentBuilding -> 
                                setCurrentBuilding running newCurrentBuilding
                            | _ -> 
                                running

                        { updatedRunningState with CurrentState = updatedPageState |> BuildingPageState |> Some }, pageCommand |> Cmd.map BuildingPageMsg |> Some
                    | _ ->
                        running, None
                Running newState, match newCmd with | Some cmd -> cmd | None -> Cmd.none
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
        | Page.OwnerDetails _
        | Page.OwnerList _ ->
            match newPage with
            | Page.OwnerDetails _
            | Page.OwnerList _ -> false
            | _ -> true
        | Page.LotDetails _
        | Page.LotList _ ->
            match newPage with
            | Page.LotDetails _
            | Page.LotList _ -> false
            | _ -> true
        | Page.ProfessionalSyndicDetails _
        | Page.ProfessionalSyndicList _ ->
            match newPage with
            | Page.ProfessionalSyndicDetails _
            | Page.ProfessionalSyndicList -> false
            | _ -> true
        | _ -> true

    let notFound = div [] [ p [] [ str "Pagina werd niet gevonden." ] ]

    let view (state: ClientState) (dispatch: ClientMessage -> unit) =
        match state with
        | Initializing ->
            div [] [                
                Navbar.render {| CurrentPage = None; CurrentUser = None; CurrentBuilding = None |}
                div [ Class Bootstrap.py3 ] [ h2 [] [ str "De app wordt gestart" ] ] 
            ]
        | Running runningState ->
            let currentPage =
                div [ classes [ Bootstrap.containerFluid; Bootstrap.py3 ] ] [
                    match runningState.CurrentPage, runningState.CurrentState, runningState.CurrentBuilding with
                    | Page.Portal, _, _ -> 
                        PortalPage.render 
                            {| 
                                CurrentUser = runningState.CurrentUser
                                CurrentBuilding = runningState.CurrentBuilding
                            |}
                    | Page.BuildingList, Some (BuildingPageState s), _ ->
                        BuildingsPage.view s (BuildingPageMsg >> dispatch)
                    | Page.BuildingDetails _, Some (BuildingPageState s), _ ->
                        BuildingsPage.view s (BuildingPageMsg >> dispatch)
                    | Page.OwnerList _, _, Some building ->
                        OwnersPage.render 
                            {| 
                                CurrentUser = runningState.CurrentUser
                                CurrentBuilding = building
                                PersonId = None
                            |}
                    | Page.OwnerDetails props, _, Some building ->
                        OwnersPage.render 
                            {| 
                                CurrentUser = runningState.CurrentUser
                                CurrentBuilding = building
                                PersonId = Some props.DetailId
                            |}
                    | Page.LotList _, _, Some building ->
                        LotsPage.render 
                            {| 
                                CurrentUser = runningState.CurrentUser
                                CurrentBuilding = building
                                LotId = None
                            |}
                    | Page.LotDetails props, _, Some building ->
                        LotsPage.render 
                            {| 
                                CurrentUser = runningState.CurrentUser
                                CurrentBuilding = building
                                LotId = Some props.DetailId
                            |}                    
                    | Page.OrganizationList _, _, Some building ->
                        OrganizationsPage.render 
                            {| 
                                CurrentUser = runningState.CurrentUser
                                CurrentBuilding = building
                                OrganizationId = None
                            |}
                    | Page.OrganizationDetails props, _, Some building ->
                        OrganizationsPage.render 
                            {| 
                                CurrentUser = runningState.CurrentUser
                                CurrentBuilding = building
                                OrganizationId = Some props.DetailId
                            |}
                    | Page.ProfessionalSyndicList, _, _ ->
                        ProfessionalSyndicsPage.render 
                            {|
                                CurrentUser = runningState.CurrentUser
                                ProfessionalSyndicId = None
                            |}
                    | Page.ProfessionalSyndicDetails proSyndicId, _, _ ->
                        ProfessionalSyndicsPage.render
                            {|
                                CurrentUser = runningState.CurrentUser
                                ProfessionalSyndicId = Some proSyndicId
                            |}
                    | Page.OrganizationTypeList, _, _ ->
                        OrganizationTypesPage.render 
                            {|
                                CurrentUser = runningState.CurrentUser
                            |}
                    | Page.Contracts _, _, Some building ->
                        ContractsPage.render 
                            {|
                                CurrentUser = runningState.CurrentUser
                                CurrentBuildingId = building.BuildingId
                            |}
                    | _ -> notFound
                ]

            div [] [
                Navbar.render 
                    {| 
                        CurrentPage = Some runningState.CurrentPage
                        CurrentUser = Some runningState.CurrentUser
                        CurrentBuilding = runningState.CurrentBuilding 
                    |}
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
                Navbar.render 
                    {| 
                        CurrentPage = None
                        CurrentUser = None
                        CurrentBuilding = None 
                    |}
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
