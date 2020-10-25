namespace Client

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
open Client.Financial.DistributionKeys
open Client.Financial.Invoicing
open Client.Users

module Client =
    importAll "./public/styles/bootstrap.min.css"
    importAll "./public/styles/fontawesome.all.min.css"
    importAll "./public/styles/flatpickr.css"

    type ClientState =
        | Initializing
        | Running of RunningState
        | Stopped of fatalError: exn
    and RunningState = {
        CurrentUser: User
        CurrentBuilding: BuildingListItem option
        CurrentPage: Page
        CurrentState: PageState option
        SidebarIsOpen: bool
        AdminModeEnabled: bool
    }
    and PageState =
        | BuildingPageState of BuildingsPage.State
    type ClientMessage =
        | CurrentUserFetched of User
        | StartApplication of User * BuildingListItem option * adminModeEnabled: bool
        | StopApplication of exn
        | PageNotFound
        | ChangePage of Routing.Page
        | CurrentBuildingChanged of BuildingListItem
        | AdminModeChanged of bool
        | ChangeCurrentBuildingAndPage of BuildingListItem * Routing.Page
        | BuildingPageMsg of BuildingsPage.Msg
        | SetSidebarDocked of bool

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

    let private setAdminMode (running: RunningState) (adminModeEnabled: bool) =
        Browser.WebStorage.localStorage.setItem("adminModeEnabled", string adminModeEnabled)
        Running { running with AdminModeEnabled = adminModeEnabled }, Cmd.ofMsg (ChangePage Page.Portal)

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
                match Library.getCachedSettings currentUser with
                | (Some currentBuilding, adminModeEnabled) -> 
                    Cmd.ofMsg (StartApplication (currentUser, Some currentBuilding, adminModeEnabled))
                | (None, adminModeEnabled) ->
                    match currentUser.Roles with
                    | [] ->
                        Cmd.ofMsg (StopApplication (exn "U heeft geen toegang tot de applicatie..."))
                    | roles ->
                        match roles |> List.tryPick getFirstBuildingId with
                        | Some buildingId -> 
                            Cmd.OfAsync.either 
                                (Client.Remoting.getRemotingApi().GetBuilding) buildingId 
                                (fun building ->
                                    building
                                    |> Option.map (fun b -> b.ToListItem())
                                    |> (fun currentBuilding -> currentUser, currentBuilding, adminModeEnabled)
                                    |> StartApplication)
                                StopApplication
                        | None ->
                            //Assume the user is a sysadmin -> pick the first building
                            Cmd.OfAsync.either
                                (Client.Remoting.getRemotingApi().GetBuildings) None
                                (List.tryHead
                                    >> (fun currentBuilding -> currentUser, currentBuilding, adminModeEnabled)
                                    >> StartApplication)
                                StopApplication
            state, newCmd
        | StartApplication (currentUser, currentBuilding, adminModeEnabled) ->
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
                SidebarIsOpen = true
                AdminModeEnabled = adminModeEnabled
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
                | Page.Contracts props                
                | Page.OrganizationList props
                | Page.DistributionKeyList props when currentBuildingId <> props.BuildingId ->
                    state, fetchBuildingAndChangePage props.BuildingId newPage
                | Page.LotDetails props
                | Page.OwnerDetails props
                | Page.OrganizationDetails props
                | Page.DistributionKeyDetails props when currentBuildingId <> props.BuildingId ->
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
        | AdminModeChanged adminModeEnabled ->
            match state with
            | Running running when running.AdminModeEnabled <> adminModeEnabled ->
                setAdminMode running adminModeEnabled
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
        | SetSidebarDocked opened ->
            match state with
            | Running runningState -> Running { runningState with SidebarIsOpen = opened }, Cmd.none
            | _ -> state, Cmd.none



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
        | Page.DistributionKeyDetails _
        | Page.DistributionKeyList _ ->
            match newPage with
            | Page.DistributionKeyDetails _
            | Page.DistributionKeyList _ -> false
            | _ -> true
        | Page.UserDetails _
        | Page.UserList ->
            match newPage with
            | Page.UserDetails _
            | Page.UserList -> false
            | _ -> true
        | _ -> true

    let notFound = div [] [ p [] [ str "Pagina werd niet gevonden." ] ]

    let view (state: ClientState) (dispatch: ClientMessage -> unit) =
        match state with
        | Initializing ->
            div [] [                
                div [ Class Bootstrap.py3 ] [ h2 [] [ str "De app wordt gestart" ] ] 
            ]
        | Running runningState ->
            let currentPage =                
                match runningState.CurrentPage, runningState.CurrentState, runningState.CurrentBuilding with
                | Page.Portal, _, _ -> 
                    PortalPage.render 
                        {| 
                            CurrentUser = runningState.CurrentUser
                            CurrentBuilding = runningState.CurrentBuilding
                            AdminModeEnabled = runningState.AdminModeEnabled
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
                | Page.FinancialSettings _, _, Some building ->
                    Financial.Settings.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            CurrentBuildingId = building.BuildingId
                        |}
                | Page.DistributionKeyList _, _, Some building ->
                    DistributionKeysPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            CurrentBuildingId = building.BuildingId
                            DistributionKeyId = None
                        |}
                | Page.DistributionKeyDetails props, _, Some building ->
                    DistributionKeysPage.render 
                        {| 
                            CurrentUser = runningState.CurrentUser
                            CurrentBuildingId = building.BuildingId
                            DistributionKeyId = Some props.DetailId
                        |}
                | Page.Invoices _, _, Some building ->
                    InvoicesPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            CurrentBuilding = building
                            InvoiceId = None
                        |}
                | Page.InvoiceDetails props, _, Some building ->
                    InvoicesPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            CurrentBuilding = building
                            InvoiceId = Some props.DetailId
                        |}
                | Page.UserList _, _, _ ->
                    UsersPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            UserId = None
                        |}
                | Page.UserDetails userId, _, _ ->
                    UsersPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            UserId = Some userId
                        |}
                | Page.NoticeBoard, _, _
                | Page.MyEvents, _, _
                | Page.MyLots, _, _
                | Page.MyContracts, _, _
                | Page.MyFinancials, _, _
                | Page.InvoiceDetails _, _, _
                | Page.Provisions _, _, _
                | Page.BankNotes _, _, _ ->
                    div [] [
                        str "Deze pagina is nog niet beschikbaar"
                    ]
                | _ -> notFound

            div [] [
                input [ Type "checkbox"; Id "melior-sidebar-toggle" ]
                header [ classes [ Bootstrap.bgDark; "melior-navbar" ] ] [                    
                    label [ HtmlFor "melior-sidebar-toggle"] [
                        span [ classes [ FontAwesome.fas; FontAwesome.faBars ]; Id "melior-sidebar-button" ] []
                    ]
                    div [ Class "melior-brand" ] [
                        h3 [] [
                            span [ Style [ Color "#007bff" ] ] [
                                str "Syndicus" 
                            ]
                            span [ Style [ Color "#54b037" ] ] [
                                str " Assistent"
                            ]
                        ]
                    ]
                    div [ Class "melior-profile" ] [
                        a [ classes [ Bootstrap.btn; Bootstrap.btnSecondary; Bootstrap.btnSm ]; Href "/authentication/logout" ] [
                            str "Afmelden"
                        ]
                    ]
                ]                
                div [ classes [ Bootstrap.navbarDark; Bootstrap.bgDark; "melior-sidebar" ];  ] [
                    Sidebar.render
                        {|
                            SidebarIsOpen = runningState.SidebarIsOpen
                            OnSetSidebarOpened = SetSidebarDocked >> dispatch
                            CurrentPage = Some runningState.CurrentPage
                            CurrentBuilding = runningState.CurrentBuilding
                            CurrentUser = runningState.CurrentUser
                            AdminModeEnabled = runningState.AdminModeEnabled
                            OnChangeAdminMode = AdminModeChanged >> dispatch
                        |}
                ]
                div [ classes [ "melior-content" ] ] [
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
            ]
        | Stopped _ ->
            div [] [
                div [ Class Bootstrap.py3 ] [ h2 [] [ str "Er is iets misgelopen bij het laden van de applicatie." ] ] 
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
