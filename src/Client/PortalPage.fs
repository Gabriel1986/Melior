module Client.PortalPage

open Shared.Domain
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents
open Routing

type State = {
    CurrentUser: CurrentUser
}

type Msg =
    | OpenPage of NavigablePage

type PortalPageProps = {| CurrentUser: CurrentUser |}

let init (props: PortalPageProps) =
    { CurrentUser = props.CurrentUser }, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | OpenPage page ->
        state, Routing.navigateToNavigablePage page

let view (state: State) (dispatch: Msg -> unit) =
    div [] [
        div [] [
            button [ OnClick (fun _ -> Msg.OpenPage BuildingList |> dispatch) ] [ str "Gebouwen" ]
            button [ OnClick (fun _ -> Msg.OpenPage ResidentList |> dispatch) ] [ str "Bewoners" ]
            button [ OnClick (fun _ -> Msg.OpenPage LotList |> dispatch) ] [ str "Kavels" ]
            button [] [ str "Organisaties (TODO)" ]
            button [] [ str "Gebruikers (TODO)" ]
        ]
    ]

let render (props: PortalPageProps) =
    React.elmishComponent ("PortalPage", init props, update, view)