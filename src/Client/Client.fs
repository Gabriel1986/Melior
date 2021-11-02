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
open Feliz
open Feliz.Router

open Shared.Read

open Routing
open Client
open Client.Library
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
open Client.Financial.FinancialTransactions
open Client.Financial.Deposits
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
        SidebarIsOpen: bool
        AdminModeEnabled: bool
        Warnings: Warning list
    }
    type ClientMessage =
        | CurrentUserFetched of User
        | StartApplication of User * BuildingListItem option * adminModeEnabled: bool
        | StopApplication of exn
        | PageNotFound
        | ChangePage of Routing.Page
        | AdminModeChanged of bool
        | ChangeCurrentBuildingAndPage of BuildingListItem * Routing.Page
        | SetSidebarDocked of bool
        | ReloadWarnings
        | WarningsLoaded of Warning list
        | RemotingException of exn

    module Server =
        let loadWarnings buildingId =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi()).GetWarnings buildingId
                WarningsLoaded
                RemotingException
    
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
            let cmd =
                match currentBuilding with
                | Some currentBuilding -> Server.loadWarnings currentBuilding.BuildingId
                | None -> Cmd.none

            Running {
                CurrentUser = currentUser
                CurrentPage = currentPage
                CurrentBuilding = currentBuilding
                SidebarIsOpen = true
                AdminModeEnabled = adminModeEnabled
                Warnings = []
            }, cmd
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
                | newPage ->
                    Running { running with CurrentPage = newPage }, Cmd.none
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

                Running { newRunningState with CurrentPage = newPage }
                , Server.loadWarnings newBuilding.BuildingId
            | _ ->
                state, Cmd.none
        | SetSidebarDocked opened ->
            match state with
            | Running runningState -> Running { runningState with SidebarIsOpen = opened }, Cmd.none
            | _ -> state, Cmd.none
        | WarningsLoaded warnings ->
            match state with
            | Running runningState -> Running { runningState with Warnings = warnings }, Cmd.none
            | _ -> state, Cmd.none
        | RemotingException err ->
            state, showGenericErrorModalCmd err
        | ReloadWarnings ->
            match state with
            | Running runningState when runningState.CurrentBuilding.IsSome ->
                state, Server.loadWarnings runningState.CurrentBuilding.Value.BuildingId
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
        | Page.DistributionKeyDetails _
        | Page.DistributionKeyList _ ->
            match newPage with
            | Page.DistributionKeyDetails _
            | Page.DistributionKeyList _ -> false
            | _ -> true
        | Page.InvoiceDetails _
        | Page.Invoices _ ->
            match newPage with
            | Page.InvoiceDetails _
            | Page.Invoices _ -> false
            | _ -> true
        | Page.DepositRequestDetails _
        | Page.DepositRequests _ ->
            match newPage with
            | Page.DepositRequestDetails _
            | Page.DepositRequests _ -> false
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
            let warningsForConcept (concept: Concept) =
                runningState.Warnings |> List.filter (fun warning -> warning.Concept = concept)
            let currentPage =
                match runningState.CurrentPage, runningState.CurrentBuilding with
                | Page.Portal, _ -> 
                    PortalPage.render 
                        {| 
                            CurrentUser = runningState.CurrentUser
                            CurrentBuilding = runningState.CurrentBuilding
                            AdminModeEnabled = runningState.AdminModeEnabled
                        |}
                | Page.BuildingList, _ ->
                    BuildingsPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            CurrentBuildingId = runningState.CurrentBuilding |> Option.map (fun b -> b.BuildingId)
                            BuildingId = None
                            OnCurrentBuildingChanged = fun building -> ChangeCurrentBuildingAndPage (building, Page.BuildingList) |> dispatch
                            Warnings = []
                        |}
                | Page.BuildingDetails buildingId, _  ->
                    BuildingsPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            CurrentBuildingId = runningState.CurrentBuilding |> Option.map (fun b -> b.BuildingId)
                            BuildingId = Some buildingId
                            OnCurrentBuildingChanged = fun building -> ChangeCurrentBuildingAndPage (building, Page.BuildingList) |> dispatch
                            Warnings = runningState.Warnings |> List.filter (fun w -> match w.Concept with | Concept.Building bId -> bId = buildingId | _ -> false)
                         |}
                | Page.OwnerList _, Some building ->
                    OwnersPage.render 
                        {| 
                            CurrentUser = runningState.CurrentUser
                            CurrentBuilding = building
                            OwnerId = None
                        |}
                | Page.OwnerDetails props, Some building ->
                    OwnersPage.render 
                        {| 
                            CurrentUser = runningState.CurrentUser
                            CurrentBuilding = building
                            OwnerId = Some props.DetailId
                        |}
                | Page.LotList _, Some building ->
                    LotsPage.render 
                        {| 
                            CurrentUser = runningState.CurrentUser
                            CurrentBuilding = building
                            LotId = None
                            OnLotsChanged = fun _ -> dispatch ReloadWarnings
                            Warnings = warningsForConcept Concept.Lot
                        |}
                | Page.LotDetails props, Some building ->
                    LotsPage.render 
                        {| 
                            CurrentUser = runningState.CurrentUser
                            CurrentBuilding = building
                            LotId = Some props.DetailId
                            OnLotsChanged = fun _ -> dispatch ReloadWarnings
                            Warnings = warningsForConcept Concept.Lot
                        |}                    
                | Page.OrganizationList _, Some building ->
                    OrganizationsPage.render 
                        {| 
                            CurrentUser = runningState.CurrentUser
                            CurrentBuilding = building
                            OrganizationId = None
                        |}
                | Page.OrganizationDetails props, Some building ->
                    OrganizationsPage.render 
                        {| 
                            CurrentUser = runningState.CurrentUser
                            CurrentBuilding = building
                            OrganizationId = Some props.DetailId
                        |}
                | Page.ProfessionalSyndicList, _ ->
                    ProfessionalSyndicsPage.render 
                        {|
                            CurrentUser = runningState.CurrentUser
                            ProfessionalSyndicId = None
                        |}
                | Page.ProfessionalSyndicDetails proSyndicId, _ ->
                    ProfessionalSyndicsPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            ProfessionalSyndicId = Some proSyndicId
                        |}
                | Page.OrganizationTypeList, _ ->
                    OrganizationTypesPage.render 
                        {|
                            CurrentUser = runningState.CurrentUser
                        |}
                | Page.Contracts _, Some building ->
                    ContractsPage.render 
                        {|
                            CurrentUser = runningState.CurrentUser
                            CurrentBuildingId = building.BuildingId
                            Warnings = warningsForConcept Concept.Contract
                            OnAnswersChanged = fun _ -> dispatch ReloadWarnings
                            OnContractsChanged = fun _ -> dispatch ReloadWarnings
                        |}
                | Page.FinancialSettings _, Some building ->
                    Financial.Settings.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            CurrentBuildingId = building.BuildingId
                        |}
                | Page.DistributionKeyList _, Some building ->
                    DistributionKeysPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            CurrentBuildingId = building.BuildingId
                            DistributionKeyId = None
                        |}
                | Page.DistributionKeyDetails props, Some building ->
                    DistributionKeysPage.render 
                        {| 
                            CurrentUser = runningState.CurrentUser
                            CurrentBuildingId = building.BuildingId
                            DistributionKeyId = Some props.DetailId
                        |}
                | Page.Invoices _, Some building ->
                    InvoicesPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            CurrentBuilding = building
                            InvoiceId = None
                        |}
                | Page.InvoiceDetails props, Some building ->
                    InvoicesPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            CurrentBuilding = building
                            InvoiceId = Some props.DetailId
                        |}
                | Page.DepositRequests _, Some building ->
                    DepositRequestsPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            CurrentBuilding = building
                            DepositRequestId = None
                        |}
                | Page.DepositRequestDetails props, Some building ->
                    DepositRequestsPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            CurrentBuilding = building
                            DepositRequestId = Some props.DetailId
                        |}
                | Page.FinancialTransactions _, Some building ->
                    FinancialTransactionsOverview.render
                        {|
                            CurrentBuilding = building
                        |}
                | Page.Balance _, Some building ->
                    Balance.render
                        {|
                            CurrentBuilding = building
                        |}
                | Page.UserList _, _ ->
                    UsersPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            UserId = None
                        |}
                | Page.UserDetails userId, _ ->
                    UsersPage.render
                        {|
                            CurrentUser = runningState.CurrentUser
                            UserId = Some userId
                        |}
                | Page.NoticeBoard, _
                | Page.MyEvents, _
                | Page.MyLots, _
                | Page.MyContracts, _
                | Page.MyFinancials, _
                | Page.BankNotes _, _ ->
                    div [] [
                        str "Deze pagina is nog niet beschikbaar"
                    ]
                | _ -> notFound

            div [] [
                input [ Type "checkbox"; Id "melior-sidebar-toggle" ]
                header [ classes [ Bootstrap.bgLight; "melior-navbar" ] ] [                    
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
                        if runningState.CurrentUser.HasAccessToAdminMode () then
                            div [ classes [ Bootstrap.btnGroup; Bootstrap.btnGroupSm ] ] [
                                button [ classes [ Bootstrap.btn; (if runningState.AdminModeEnabled then Bootstrap.btnSecondary else Bootstrap.btnOutlineSecondary) ]; OnClick (fun _ -> AdminModeChanged true |> dispatch) ] [
                                    i [ classes [ FontAwesome.fa; FontAwesome.faUserCog ] ] []
                                    str " "
                                    str "Admin"
                                ]
                                button [ classes [ Bootstrap.btn; (if runningState.AdminModeEnabled then Bootstrap.btnOutlineSecondary else Bootstrap.btnSecondary) ]; OnClick (fun _ -> AdminModeChanged false |> dispatch) ] [
                                    i [ classes [ FontAwesome.fa; FontAwesome.faUser ] ] []
                                    str " "
                                    str "Gebruiker"
                                ]
                            ]
                            
                        a [ classes [ Bootstrap.btn; Bootstrap.btnSecondary; Bootstrap.btnSm; Bootstrap.ml3 ]; Href "/authentication/logout" ] [
                            i [ classes [ FontAwesome.fa; FontAwesome.faSignOutAlt ] ] []
                            str " "
                            str "Afmelden"
                        ]
                    ]
                ]
                div [ classes [ Bootstrap.navbarLight; Bootstrap.bgLight; "melior-sidebar" ] ] [
                    Sidebar.render
                        {|
                            SidebarIsOpen = runningState.SidebarIsOpen
                            OnSetSidebarOpened = SetSidebarDocked >> dispatch
                            CurrentPage = Some runningState.CurrentPage
                            CurrentBuilding = runningState.CurrentBuilding
                            CurrentUser = runningState.CurrentUser
                            AdminModeEnabled = runningState.AdminModeEnabled
                            Warnings = runningState.Warnings
                        |}
                ]
                div [ classes [ "melior-content" ] ] [
                    React.router [
                        router.onUrlChanged (fun urlParts ->
                            let newPage = Routing.parseUrl urlParts
                            if runningState.CurrentPage = newPage then ()
                            elif shouldChangePage runningState.CurrentPage newPage then ChangePage newPage |> dispatch
                            else ()
                        )
                        router.children [ currentPage ]
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
