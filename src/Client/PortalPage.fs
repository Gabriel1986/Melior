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
    CurrentBuildingId: Guid option
}

type Msg =
    | OpenPage of NavigablePage

type PortalPageProps = {| CurrentUser: CurrentUser; CurrentBuildingId: Guid option |}

let init (props: PortalPageProps) =
    { CurrentUser = props.CurrentUser; CurrentBuildingId = props.CurrentBuildingId }, Cmd.none

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
                    match state.CurrentBuildingId with
                    | Some currentBuildingId -> [
                            button
                                [ OnClick (fun _ -> Msg.OpenPage (LotList { BuildingId = currentBuildingId }) |> dispatch) ]
                                [ str "Kavels" ]
                            button
                                [ OnClick (fun _ -> Msg.OpenPage (OwnerList { BuildingId = currentBuildingId }) |> dispatch) ]
                                [ str "Eigenaars" ]
                            button
                                [ OnClick (fun _ -> Msg.OpenPage (OrganizationList { BuildingId = currentBuildingId }) |> dispatch) ]
                                [ str "Organisaties" ]
                            button
                                []
                                [ str "Gebruikers (TODO)" ]
                        ]
                    | None ->
                        []
        ]
    ]

let render (props: PortalPageProps) =
    React.elmishComponent ("PortalPage", init props, update, view)