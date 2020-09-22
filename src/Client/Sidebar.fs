module Client.Sidebar

open System
open Elmish
open Fable
open Feliz
open Feliz.ElmishComponents
open Fable.React
open Fable.React.Props

open ClientStyle
open ClientStyle.Helpers
open Routing
open Shared.Read

type State = {
    SidebarIsOpen: bool
    CurrentBuilding: BuildingListItem option
    CurrentUser: User
    AdminModeEnabled: bool
    FinancialSubmenuIsOpen: bool
}

type Msg =
    | SetOpen of bool
    | NavigateToPage of Page
    | ToggleFinancialSubmenu
    | SetAdminMode
    | SetUserMode

let init (isOpen: bool) (currentBuilding: BuildingListItem option) (currentUser: User) (adminModeEnabled: bool) =
    { 
        SidebarIsOpen = isOpen 
        CurrentBuilding = currentBuilding
        CurrentUser = currentUser
        AdminModeEnabled = adminModeEnabled
        FinancialSubmenuIsOpen = false
    }, Cmd.none

let update onSetSidebarOpened onChangeAdminMode (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | SetOpen opened ->
        onSetSidebarOpened(opened)
        { state with SidebarIsOpen = opened }, Cmd.none

    | NavigateToPage page ->
        state, Routing.navigateToPage page

    | SetAdminMode ->
        onChangeAdminMode true
        state, Cmd.none
    
    | SetUserMode ->
        onChangeAdminMode false
        state, Cmd.none

    | ToggleFinancialSubmenu ->
        { state with FinancialSubmenuIsOpen = not state.FinancialSubmenuIsOpen }, Cmd.none

let determineStyle selectedPage page =
    let extraClasses =
        match selectedPage with
        | Some currentNavigablePage when page = currentNavigablePage -> 
            [ Bootstrap.active ]
        | _ -> 
            []
    String.Join(" ", Bootstrap.navLink::extraClasses)

let convertCurrentPageForNavigation page =
    match page with
    | Some (Page.Portal)                      -> Some (Page.Portal)
    | Some (Page.ProfessionalSyndicList)
    | Some (Page.ProfessionalSyndicDetails _) -> Some (Page.ProfessionalSyndicList)
    | Some (Page.OrganizationTypeList)        -> Some (Page.OrganizationTypeList)
    | Some (Page.BuildingList)                
    | Some (Page.BuildingDetails _)           -> Some (Page.BuildingList)
    | Some (Page.OwnerList p)                 -> Some (Page.OwnerList p)        
    | Some (Page.OwnerDetails p)              -> Some (Page.OwnerList { BuildingId = p.BuildingId })
    | Some (Page.LotList p)                   -> Some (Page.LotList p)
    | Some (Page.LotDetails p)                -> Some (Page.LotList { BuildingId = p.BuildingId })
    | Some (Page.OrganizationList p)          -> Some (Page.OrganizationList p)
    | Some (Page.OrganizationDetails p)       -> Some (Page.OrganizationList { BuildingId = p.BuildingId })
    | Some (Page.Contracts p)                 -> Some (Page.Contracts p)
    | Some (Page.NoticeBoard)                 -> Some (Page.NoticeBoard)
    | Some (Page.MyEvents)                    -> Some (Page.MyEvents)
    | Some (Page.MyLots)                      -> Some (Page.MyLots)
    | Some (Page.MyContracts)                 -> Some (Page.MyContracts)
    | Some (Page.MyFinancials)                -> Some (Page.MyFinancials)
    | Some (Page.DistributionKeyList p)       -> Some (Page.DistributionKeyList p)
    | Some (Page.DistributionKeyDetails p)    -> Some (Page.DistributionKeyList { BuildingId = p.BuildingId })
    | Some (Page.CostDiary p)                 -> Some (Page.CostDiary p)
    | Some (Page.InvoiceDetails p)            -> Some (Page.CostDiary { BuildingId = p.BuildingId })
    | Some (Page.FinancialDiary p)            -> Some (Page.FinancialDiary p)
    | Some (Page.IncomeDiary p)               -> Some (Page.IncomeDiary p)
    | Some (Page.NotFound)
    | None                                    -> None

let renderAdminMode (state: State) (currentPage: Page option) (dispatch: Msg -> unit) =
    let userHasAccessToMultipleBuildings =
        state.CurrentUser.Roles |> List.exists (function | ProfessionalSyndicRole _ | SysAdminRole -> true | UserRole _ | SyndicRole _ -> false) 

    [
        if userHasAccessToMultipleBuildings then
            yield
                li [ Class Bootstrap.navItem ] [
                    a [ 
                        Class (determineStyle currentPage Page.BuildingList)
                        OnClick (fun _ -> NavigateToPage Page.BuildingList |> dispatch) 
                    ] [ str "Gebouwen" ]
                ]

        if state.CurrentBuilding.IsSome then yield! [
            //yield li [] [
            //    span [ Class Bootstrap.navbarText ] [
            //        str state.CurrentBuilding.Value.Name
            //    ]
            //    span [ Class Bootstrap.navbarText ] [
            //        str (sprintf "(%s)" state.CurrentBuilding.Value.Code)
            //    ]
            //]

            let buildingSpecificProps = { BuildingId = state.CurrentBuilding.Value.BuildingId }
            yield li [ Class Bootstrap.navItem ] [
                a [
                    Class (determineStyle currentPage (Page.OwnerList buildingSpecificProps))
                    OnClick (fun _ -> NavigateToPage (Page.OwnerList buildingSpecificProps) |> dispatch)
                ] [ str "Eigenaars" ]
            ]
            yield li [ Class Bootstrap.navItem ] [
                a [
                    Class (determineStyle currentPage (Page.LotList buildingSpecificProps))
                    OnClick (fun _ -> NavigateToPage (Page.LotList buildingSpecificProps) |> dispatch)
                ] [ str "Kavels" ]
            ]
            yield li [ Class Bootstrap.navItem ] [
                a [
                    Class (determineStyle currentPage (Page.OrganizationList buildingSpecificProps))
                    OnClick (fun _ -> NavigateToPage (Page.OrganizationList buildingSpecificProps) |> dispatch)
                ] [ str "Leveranciers" ]
            ]
            yield li [ Class Bootstrap.navItem ] [
                a [
                    Class (determineStyle currentPage (Page.Contracts buildingSpecificProps))
                    OnClick (fun _ -> NavigateToPage (Page.Contracts buildingSpecificProps) |> dispatch)                
                ] [ str "Contracten" ]
            ]
            yield li [ Class Bootstrap.navItem ] [
                a [
                    Class Bootstrap.navLink
                    OnClick (fun _ -> ToggleFinancialSubmenu |> dispatch)
                ] [ str "Boekhouding" ]

                let currentPageIsFinancialPage =
                    match currentPage with 
                    | Some(Page.IncomeDiary _) 
                    | Some(Page.CostDiary _) 
                    | Some(Page.FinancialDiary _) -> true | _ -> false

                if state.FinancialSubmenuIsOpen || currentPageIsFinancialPage then
                    ul [ classes [ Bootstrap.navbarNav; Bootstrap.flexColumn; Bootstrap.px4 ] ] [
                        li [ Class Bootstrap.navItem ] [
                            a [
                                Class (determineStyle currentPage (Page.IncomeDiary buildingSpecificProps))
                                OnClick (fun _ -> NavigateToPage (Page.IncomeDiary buildingSpecificProps) |> dispatch)
                            ] [ str "Provisies" ]
                        ]
                        li [ Class Bootstrap.navItem ] [
                            a [
                                Class (determineStyle currentPage (Page.CostDiary buildingSpecificProps))
                                OnClick (fun _ -> NavigateToPage (Page.CostDiary buildingSpecificProps) |> dispatch)
                            ] [ str "Facturen" ]
                        ]
                        li [ Class Bootstrap.navItem ] [
                            a [
                                Class (determineStyle currentPage (Page.FinancialDiary buildingSpecificProps))
                                OnClick (fun _ -> NavigateToPage (Page.FinancialDiary buildingSpecificProps) |> dispatch)
                            ] [ str "Bankuittreksels" ]
                        ]
                    ]
                else
                    null
            ]
        ]
    ]

let renderUserMode (state: State) (currentPage: Page option) (dispatch: Msg -> unit) = 
    [
        yield li [ Class Bootstrap.navItem ] [
            a [
                Class (determineStyle currentPage Page.NoticeBoard)
                OnClick (fun _ -> NavigateToPage Page.NoticeBoard |> dispatch)
            ] [ str "Prikbord" ]
        ]
        //yield li [ Class Bootstrap.navItem ] [
        //    a [
        //        Class (determineStyle currentPage Page.MyEvents)
        //        OnClick (fun _ -> NavigateToPage Page.MyEvents |> dispatch)
        //    ] [ str "Evenementen" ]
        //]
        yield li [ Class Bootstrap.navItem ] [
            a [
                Class (determineStyle currentPage Page.MyLots)
                OnClick (fun _ -> NavigateToPage (Page.MyLots) |> dispatch)
            ] [ str "Kavels" ]
        ]
        yield li [ Class Bootstrap.navItem ] [
            a [
                Class (determineStyle currentPage Page.MyContracts)
                OnClick (fun _ -> NavigateToPage Page.MyContracts |> dispatch)                
            ] [ str "Contracten" ]
        ]
    ]


let renderNavigation (state: State) (currentPage: Page option) (dispatch: Msg -> unit) = 
    [
        if state.CurrentUser.HasAccessToAdminMode () then
            yield li [ Class Bootstrap.navItem ] [
                div [ classes [ Bootstrap.btnGroup; Bootstrap.btnGroupSm ] ] [
                    button [ classes [ Bootstrap.btn; (if state.AdminModeEnabled then Bootstrap.btnPrimary else Bootstrap.btnLight) ]; OnClick (fun _ -> dispatch SetAdminMode) ] [
                        str "Admin"
                    ]
                    button [ classes [ Bootstrap.btn; (if state.AdminModeEnabled then Bootstrap.btnLight else Bootstrap.btnPrimary) ]; OnClick (fun _ -> dispatch SetUserMode) ] [
                        str "Gebruiker"
                    ]
                ]
            ]

        yield
            li [ Class Bootstrap.navItem; Style [ PaddingTop "15px" ] ] [ 
                a [ 
                    Class (determineStyle currentPage Page.Portal)
                    OnClick (fun _ -> NavigateToPage Page.Portal |> dispatch) 
                ] [ str "Portaal" ]
            ]

        if state.AdminModeEnabled 
        then
            yield! renderAdminMode state currentPage dispatch
        else
            yield! renderUserMode state currentPage dispatch
    ]

let view (currentPage: Page option) (state: State) (dispatch: Msg -> unit) =
    ul [ classes [ Bootstrap.navbarNav; Bootstrap.flexColumn; Bootstrap.px2 ] ] [
        yield! renderNavigation state (convertCurrentPageForNavigation currentPage) dispatch
    ]

let render (props: {| OnSetSidebarOpened: bool -> unit; SidebarIsOpen: bool; CurrentBuilding: BuildingListItem option; CurrentUser: User; CurrentPage: Page option; AdminModeEnabled: bool; OnChangeAdminMode: bool -> unit |}) =
    React.elmishComponent ("Sidebar", init props.SidebarIsOpen props.CurrentBuilding props.CurrentUser props.AdminModeEnabled, update props.OnSetSidebarOpened props.OnChangeAdminMode, view props.CurrentPage)