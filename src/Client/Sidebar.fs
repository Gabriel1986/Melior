module Client.Sidebar

open System
open Elmish
open Fable
open Feliz
open Feliz.ElmishComponents
open Fable.React
open Fable.React.Props

open Client.Library
open ClientStyle
open ClientStyle.Helpers
open Routing
open Shared.Library
open Shared.Read
open Shared.MediaLibrary

type State = {
    OnSetSidebarOpened: bool -> unit
    SidebarIsOpen: bool
    CurrentBuilding: BuildingListItem option
    CurrentUser: User
    CurrentPage: Page option
    AdminModeEnabled: bool
    FinancialSubmenuIsOpen: bool
    Warnings: Warning list
}

type Msg =
    | SetOpen of bool
    | NavigateToPage of Page
    | ToggleFinancialSubmenu

type SidebarProps =
    {|
        OnSetSidebarOpened: bool -> unit
        SidebarIsOpen: bool
        CurrentBuilding: BuildingListItem option
        CurrentUser: User
        CurrentPage: Page option
        AdminModeEnabled: bool
        Warnings: Warning list
    |}

let init (props: SidebarProps) =
    { 
        OnSetSidebarOpened = props.OnSetSidebarOpened
        SidebarIsOpen = props.SidebarIsOpen
        CurrentBuilding = props.CurrentBuilding
        CurrentUser = props.CurrentUser
        CurrentPage = props.CurrentPage
        AdminModeEnabled = props.AdminModeEnabled
        FinancialSubmenuIsOpen = false
        Warnings = props.Warnings
    }, Cmd.none

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | SetOpen opened ->
        state.OnSetSidebarOpened(opened)
        { state with SidebarIsOpen = opened }, Cmd.none
    | NavigateToPage page ->
        state, Routing.navigateToPage page
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
    | Some (Page.Invoices p)                  -> Some (Page.Invoices p)
    | Some (Page.InvoiceDetails p)            -> Some (Page.Invoices { BuildingId = p.BuildingId })
    | Some (Page.FinancialSettings p)         -> Some (Page.FinancialSettings { BuildingId = p.BuildingId })
    | Some (Page.BankNotes p)                 -> Some (Page.BankNotes p)
    | Some (Page.Provisions p)                -> Some (Page.Provisions p)
    | Some (Page.UserList)
    | Some (Page.UserDetails _)               -> Some (Page.UserList)
    | Some (Page.NotFound)
    | None                                    -> None

let getWarningsForConcept (concept: Concept) (state: State) =
    state.Warnings 
    |> List.filter (fun warning -> warning.Concept = concept)

let renderAdminMode (state: State) (currentPage: Page option) (dispatch: Msg -> unit) =
    let userHasAccessToMultipleBuildings =
        state.CurrentUser.Roles |> List.exists (function | ProfessionalSyndicRole _ | SysAdminRole -> true | UserRole _ | SyndicRole _ -> false) 

    [
        if state.CurrentUser.IsSysAdmin () then
            yield
                li [ Class Bootstrap.navItem ] [
                    a [
                        Class (determineStyle currentPage (Page.UserList))
                        OnClick (fun _ -> NavigateToPage (Page.UserList) |> dispatch)
                    ] [ str "Gebruikers" ]
                ]

        if userHasAccessToMultipleBuildings then
            yield
                li [ Class Bootstrap.navItem ] [
                    a [ 
                        Class (determineStyle currentPage Page.BuildingList)
                        OnClick (fun _ -> NavigateToPage Page.BuildingList |> dispatch) 
                    ] [ str "Gebouwen" ]
                ]

        if state.CurrentBuilding.IsSome then yield! [
            let buildingSpecificProps = { BuildingId = state.CurrentBuilding.Value.BuildingId }
            yield li [ Class Bootstrap.navItem ] [
                a [
                    Class (determineStyle currentPage (Page.OwnerList buildingSpecificProps))
                    OnClick (fun _ -> NavigateToPage (Page.OwnerList buildingSpecificProps) |> dispatch)
                ] [ str "Eigenaars" ]
            ]
            yield li [ Class Bootstrap.navItem ] [
                let warnings = getWarningsForConcept Concept.Lot state
                a [
                    Class (determineStyle currentPage (Page.LotList buildingSpecificProps))
                    OnClick (fun _ -> NavigateToPage (Page.LotList buildingSpecificProps) |> dispatch)
                    Title (warnings |> List.map (fun warning -> warning.Message) |> String.joinWith ", ")
                ] [
                    match warnings with
                    | [] -> null
                    | _ -> 
                        [
                            i [ classes [ FontAwesome.fa; FontAwesome.faExclamationTriangle ] ] []
                            str " "
                        ]
                        |> fragment []
                    str "Kavels"
                ]
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
                    classes [ Bootstrap.navLink; if state.FinancialSubmenuIsOpen then "nav-item-open" else null ]
                    OnClick (fun _ -> ToggleFinancialSubmenu |> dispatch)
                ] [ str "Boekhouding" ]

                let currentPageIsFinancialPage =
                    match currentPage with 
                    | Some(Page.FinancialSettings _)
                    | Some(Page.DistributionKeyList _)
                    | Some(Page.Provisions _) 
                    | Some(Page.Invoices _) 
                    | Some(Page.BankNotes _) -> true | _ -> false

                if state.FinancialSubmenuIsOpen || currentPageIsFinancialPage then
                    ul [ classes [ Bootstrap.navbarNav; Bootstrap.flexColumn; Bootstrap.ml4 ] ] [
                        li [ Class Bootstrap.navItem ] [
                            a [
                                Class (determineStyle currentPage (Page.FinancialSettings buildingSpecificProps))
                                OnClick (fun _ -> NavigateToPage (Page.FinancialSettings buildingSpecificProps) |> dispatch)
                            ] [ str "Instellingen" ]
                        ]
                        li [ Class Bootstrap.navItem ] [
                            a [
                                Class (determineStyle currentPage (Page.DistributionKeyList buildingSpecificProps))
                                OnClick (fun _ -> NavigateToPage (Page.DistributionKeyList buildingSpecificProps) |> dispatch)
                            ] [ str "Verdeelsleutels" ]
                        ]
                        li [ Class Bootstrap.navItem ] [
                            a [
                                Class (determineStyle currentPage (Page.Provisions buildingSpecificProps))
                                OnClick (fun _ -> NavigateToPage (Page.Provisions buildingSpecificProps) |> dispatch)
                            ] [ str "Provisies" ]
                        ]
                        li [ Class Bootstrap.navItem ] [
                            a [
                                Class (determineStyle currentPage (Page.Invoices buildingSpecificProps))
                                OnClick (fun _ -> NavigateToPage (Page.Invoices buildingSpecificProps) |> dispatch)
                            ] [ str "Facturen" ]
                        ]
                        li [ Class Bootstrap.navItem ] [
                            a [
                                Class (determineStyle currentPage (Page.BankNotes buildingSpecificProps))
                                OnClick (fun _ -> NavigateToPage (Page.BankNotes buildingSpecificProps) |> dispatch)
                            ] [ str "Bankuittreksels" ]
                        ]
                    ]
                else
                    null
            ]
        ]
    ]
    |> fragment []

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
    |> fragment []


let renderNavigation (state: State) (currentPage: Page option) (dispatch: Msg -> unit) = 
    let pictureId = Hooks.useState (state.CurrentBuilding |> Option.bind (fun cb -> cb.PictureId))

    [
        if state.CurrentUser.HasAccessToAdminMode() && state.AdminModeEnabled && pictureId.current.IsSome then
            li [] [
                img [
                    Src (Client.Upload.thumbnailUri Partitions.BuildingImages pictureId.current.Value) 
                    Alt "Building image"
                    Style [ MaxWidth "142px"; Border "2px black solid" ]
                    OnError (fun _ -> pictureId.update(fun _ -> None))
                    Title state.CurrentBuilding.Value.Name
                ]
            ]


        
        li [ Class Bootstrap.navItem; Style [ PaddingTop "15px" ] ] [ 
            a [ 
                Class (determineStyle currentPage Page.Portal)
                OnClick (fun _ -> NavigateToPage Page.Portal |> dispatch) 
            ] [ str "Portaal" ]
        ]

        if state.AdminModeEnabled 
        then
            renderAdminMode state currentPage dispatch
        else
            renderUserMode state currentPage dispatch
    ]

let view (state: State) (dispatch: Msg -> unit) =
    ul [ classes [ Bootstrap.navbarNav; Bootstrap.flexColumn; Bootstrap.px2 ] ] [
        yield! renderNavigation state (convertCurrentPageForNavigation state.CurrentPage) dispatch
    ]

let render (props: SidebarProps) =
    React.elmishComponent ("Sidebar", init props, update, view)