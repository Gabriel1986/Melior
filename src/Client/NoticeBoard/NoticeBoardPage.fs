module Client.NoticeBoardPage

open System
open Elmish
open Elmish.SweetAlert
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Read
open Shared.Remoting

open Client
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.SortableTable
open Client.Library

type State = {
    Foo: string
}

type Msg =
    | DoNothing


type NoticeBoardPageProps = {|
    Bar: string
|}
let init (props: NoticeBoardPageProps) =
    { Foo = props.Bar }, Cmd.none

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    state, Cmd.none

let view (state: State) (dispatch: Msg -> unit) =
    div [] [ str "Page under construction" ]
    |> withPageHeader "Noticeboard"

let render (props: NoticeBoardPageProps) =
    React.elmishComponent ("NoticeBoardPage", init props, update, view)