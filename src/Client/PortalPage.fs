module Client.PortalPage

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents
open Routing
open Shared.Domain

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

let view (state: State) (dispatch: Msg -> unit) =
    div [] [
        div [] [
                yield
                    button
                        [ OnClick (fun _ -> Msg.OpenPage BuildingList |> dispatch) ]
                        [ str "Gebouwen" ]
                yield!
                    match state.CurrentBuilding with
                    | Some currentBuilding -> [
                            button
                                [ OnClick (fun _ -> Msg.OpenPage (LotList { BuildingId = currentBuilding.BuildingId }) |> dispatch) ]
                                [ str (sprintf "Kavels van %s" currentBuilding.Code) ]
                            button
                                [ OnClick (fun _ -> Msg.OpenPage (OwnerList { BuildingId = currentBuilding.BuildingId }) |> dispatch) ]
                                [ str (sprintf "Eigenaars van %s" currentBuilding.Code) ]
                            button
                                [ OnClick (fun _ -> Msg.OpenPage (OrganizationList { BuildingId = currentBuilding.BuildingId }) |> dispatch) ]
                                [ str (sprintf "Organisaties van %s" currentBuilding.Code) ]
                        ]
                    | None ->
                        []
        ]
    ]

let render (props: PortalPageProps) =
    React.elmishComponent ("PortalPage", init props, update, view)