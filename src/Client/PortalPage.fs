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
    CurrentUser: CurrentUser
    CurrentBuilding: BuildingListItem option
}

type Msg =
    | OpenPage of NavigablePage

type PortalPageProps = {| CurrentUser: CurrentUser; CurrentBuilding: BuildingListItem option |}

let init (props: PortalPageProps) =
    { CurrentUser = props.CurrentUser; CurrentBuilding = props.CurrentBuilding }, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | OpenPage page ->
        state, Routing.navigateToNavigablePage page

let private inCard (body: string) (imgUrl: string) (element: ReactElement) =
    div [ Class Bootstrap.card ] [
        img [ Class Bootstrap.cardImgTop; Src imgUrl; Alt "Image missing :(" ]
        div [ Class Bootstrap.cardBody ] [
            p [ Class Bootstrap.cardText ] [ str body ]
            element
        ]
    ]

let private inCol (element: ReactElement) =
    div [ Class Bootstrap.col ] [
        element
    ]

let view (state: State) (dispatch: Msg -> unit) =
    div [] [
        div [ classes [ Bootstrap.row; Bootstrap.rowColsLg4; Bootstrap.rowCols2 ] ] [
                yield
                    button [ 
                        OnClick (fun _ -> Msg.OpenPage BuildingList |> dispatch) 
                        classes [ Bootstrap.btn; Bootstrap.btnLink ]
                    ] [ 
                        i [ classes [ FontAwesome.fa; FontAwesome.faExternalLinkAlt ] ] []
                        str " "
                        str "Gebouwen" 
                    ]
                    |> inCard "Gebouwen onder uw beheer" "https://i.ibb.co/rQnJ0hn/architecture-768432-640.jpg"
                    |> inCol
                yield!
                    match state.CurrentBuilding with
                    | Some currentBuilding -> [
                            button [ 
                                OnClick (fun _ -> Msg.OpenPage (LotList { BuildingId = currentBuilding.BuildingId }) |> dispatch) 
                                classes [ Bootstrap.btn; Bootstrap.btnLink ]
                            ] [ 
                                i [ classes [ FontAwesome.fa; FontAwesome.faExternalLinkAlt ] ] []
                                str " "
                                str "Kavels" 
                            ]
                            |> inCard (sprintf "De verschillende kavels die horen tot gebouw %s" currentBuilding.Code) "https://i.ibb.co/fXtJZQ0/floor-plan-1474454-640.jpg"
                            |> inCol
                            button [ 
                                OnClick (fun _ -> Msg.OpenPage (OwnerList { BuildingId = currentBuilding.BuildingId }) |> dispatch) 
                                classes [ Bootstrap.btn; Bootstrap.btnLink ]
                            ] [ 
                                i [ classes [ FontAwesome.fa; FontAwesome.faExternalLinkAlt ] ] []
                                str " "
                                str "Eigenaars"
                            ]
                            |> inCard (sprintf "De eigenaars van kavels die horen tot gebouw %s" currentBuilding.Code) "https://i.ibb.co/nbnkx9P/keys-2251770-640.jpg"
                            |> inCol
                            button [ 
                                OnClick (fun _ -> Msg.OpenPage (OrganizationList { BuildingId = currentBuilding.BuildingId }) |> dispatch) 
                                classes [ Bootstrap.btn; Bootstrap.btnLink ]
                            ] [
                                i [ classes [ FontAwesome.fa; FontAwesome.faExternalLinkAlt ] ] []
                                str " "
                                str "Organisaties"  
                            ]
                            |> inCard (sprintf "De organisaties (leveranciers, netbeheerders, ...) die diensten verlenen voor gebouw %s" currentBuilding.Code) "https://i.ibb.co/wzsz0wp/hand-4053806-640.jpg"
                            |> inCol
                        ]
                    | None ->
                        []
        ]
    ]

let render (props: PortalPageProps) =
    React.elmishComponent ("PortalPage", init props, update, view)