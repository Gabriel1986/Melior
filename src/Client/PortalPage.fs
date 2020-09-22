module Client.PortalPage

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents
open Routing
open Shared.Read
open Client.ClientStyle
open Client.ClientStyle.Helpers

type State = {
    CurrentUser: User
    CurrentBuilding: BuildingListItem option
    AdminModeEnabled: bool
}

type Msg =
    | OpenPage of Page

type PortalPageProps = {| CurrentUser: User; CurrentBuilding: BuildingListItem option; AdminModeEnabled: bool |}

let init (props: PortalPageProps) =
    { CurrentUser = props.CurrentUser; CurrentBuilding = props.CurrentBuilding; AdminModeEnabled = props.AdminModeEnabled }, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | OpenPage page ->
        state, Routing.navigateToPage page

let private inCard (imgUrl: string) (onClick: unit -> unit) (element: ReactElement) =
    div [ Class Bootstrap.card ] [
        img [ classes [ Bootstrap.cardImgTop; "pointer" ]; Src imgUrl; Alt "Image missing :("; OnClick (fun _ -> onClick()) ]
        div [ Class Bootstrap.cardBody ] [
            element
        ]
    ]

let private inCol (element: ReactElement) =
    div [ Class Bootstrap.col ] [
        element
    ]

let private portalButton (btnText: string) (onClick: unit -> unit) =
    button [ 
        OnClick (fun _ -> onClick()) 
        classes [ Bootstrap.btn; Bootstrap.btnBlock; Bootstrap.btnDark ]
    ] [
        str btnText
    ]

let view (state: State) (dispatch: Msg -> unit) =
    div [] [
        div [ classes [ Bootstrap.row; Bootstrap.rowColsLg4; Bootstrap.rowCols2 ] ] [
            if state.AdminModeEnabled then yield! [
                yield
                    portalButton "Gebouwen" (fun () -> Msg.OpenPage BuildingList |> dispatch)
                    |> inCard "https://i.ibb.co/rQnJ0hn/architecture-768432-640.jpg" (fun () -> Msg.OpenPage BuildingList |> dispatch)
                    |> inCol
                yield!
                    match state.CurrentBuilding with
                    | Some currentBuilding -> [
                            portalButton "Eigenaars" (fun () -> Msg.OpenPage (OwnerList { BuildingId = currentBuilding.BuildingId }) |> dispatch) 
                            |> inCard "https://i.ibb.co/nbnkx9P/keys-2251770-640.jpg" (fun () -> Msg.OpenPage (OwnerList { BuildingId = currentBuilding.BuildingId }) |> dispatch)
                            |> inCol

                            portalButton "Kavels" (fun () -> Msg.OpenPage (LotList { BuildingId = currentBuilding.BuildingId }) |> dispatch)
                            |> inCard "https://i.ibb.co/fXtJZQ0/floor-plan-1474454-640.jpg" (fun () -> Msg.OpenPage (LotList { BuildingId = currentBuilding.BuildingId }) |> dispatch)
                            |> inCol

                            portalButton "Leveranciers" (fun () -> Msg.OpenPage (OrganizationList { BuildingId = currentBuilding.BuildingId }) |> dispatch) 
                            |> inCard "https://i.ibb.co/sCb1XVr/hand.png" (fun () -> Msg.OpenPage (OrganizationList { BuildingId = currentBuilding.BuildingId }) |> dispatch)
                            |> inCol

                            portalButton "Contracten" (fun () -> Msg.OpenPage (Contracts { BuildingId = currentBuilding.BuildingId }) |> dispatch)
                            |> inCard "https://i.ibb.co/7N8J4wT/writing-640.jpg" (fun () -> Msg.OpenPage (Contracts { BuildingId = currentBuilding.BuildingId }) |> dispatch)
                            |> inCol

                            portalButton "Boekhouding" (fun () -> Msg.OpenPage (CostDiary { BuildingId = currentBuilding.BuildingId }) |> dispatch)
                            |> inCard "https://i.ibb.co/803ZJF3/coins-1726618-640.jpg" (fun () -> Msg.OpenPage (CostDiary { BuildingId = currentBuilding.BuildingId }) |> dispatch)
                            |> inCol
                        ]
                    | None ->
                        []
            ] else yield! [
                portalButton "Prikbord" (fun () -> Msg.OpenPage NoticeBoard |> dispatch)
                |> inCard "https://i.ibb.co/wB1YWSf/tacks-949144-640.jpg" (fun () -> Msg.OpenPage NoticeBoard |> dispatch)
                |> inCol

                //portalButton "Evenementen" (fun () -> Msg.OpenPage MyEvents |> dispatch)
                //|> inCard "https://i.ibb.co/wCQtBPs/Max-Pixel-net-Schedule-Monthly-Organizer-Day-Planner-Calendar-828611-640.jpg" (fun () -> Msg.OpenPage MyEvents |> dispatch)
                //|> inCol

                portalButton "Kavels" (fun () -> Msg.OpenPage MyLots |> dispatch)
                |> inCard "https://i.ibb.co/fXtJZQ0/floor-plan-1474454-640.jpg" (fun () -> Msg.OpenPage MyLots |> dispatch)
                |> inCol

                portalButton "Contracten" (fun () -> Msg.OpenPage MyContracts |> dispatch)
                |> inCard "https://i.ibb.co/7N8J4wT/writing-640.jpg" (fun () -> Msg.OpenPage MyContracts |> dispatch)
                |> inCol
            ]
        ]
    ]

let render (props: PortalPageProps) =
    React.elmishComponent ("PortalPage", init props, update, view)