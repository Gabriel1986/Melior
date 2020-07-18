module Client.Contracts.ContractTypesPage

open System
open Elmish
open Fable.React
open Feliz
open Feliz.ElmishComponents
open Shared.Read

type State = {
    todo: string
}

type Message =
    | TODO

type ContractsTypesPageProps = {
    CurrentUser: CurrentUser
}

let init props =
    { todo = "Balen" }, Cmd.none

let update (msg: Message) (state: State): State * Cmd<Message> =
    match msg with
    | TODO ->
        state, Cmd.none

let view (state: State) (dispatch: Message -> unit) =
    div [] [
        str "TODO..."
    ]

let render (props: ContractsTypesPageProps) =
    React.elmishComponent ("ContractsPage", init props, update, view)