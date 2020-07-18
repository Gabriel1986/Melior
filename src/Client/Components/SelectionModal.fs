namespace Client.Components

open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Library

module SelectionList =
    type Model<'T> = {
        AllItems: 'T list
        SelectionMode: SelectionMode
        SelectedItems: 'T list
    }
    and SelectionListProps<'T> = {
        SelectionMode: SelectionMode
        LoadItems: unit -> Async<'T list>
        SelectedItems: 'T list
        DisplayListItem: 'T -> ReactElement
        OnSelectionChanged: 'T list -> unit
    }
    and SelectionMode =
        | SingleSelect
        | MultiSelect

    type Message<'T> =
        | ItemsLoaded of 'T list
        | RemotingFailure of exn
        | ToggleItemSelection of 'T

    let init<'T> (props: SelectionListProps<'T>) =
        { 
            SelectionMode = props.SelectionMode
            AllItems = []
            SelectedItems = props.SelectedItems
        }, Cmd.OfAsync.either
            props.LoadItems ()
            ItemsLoaded
            RemotingFailure

    let update<'T when 'T : equality> (onSelectionChanged: 'T list -> unit) (msg: Message<'T>) (model: Model<'T>): Model<'T> * Cmd<Message<'T>> = 
        match msg with
        | ItemsLoaded items ->
            { model with AllItems = items }, Cmd.none
        | RemotingFailure e ->
            model, showGenericErrorModalCmd e
        | ToggleItemSelection item ->
            let selectedItems =
                match model.SelectionMode with
                | SingleSelect ->
                    [ item ]
                | MultiSelect ->
                    item::(model.AllItems |> List.filter (fun i -> i <> item))
            onSelectionChanged selectedItems
            { model with SelectedItems = selectedItems }, Cmd.none

    let view<'T when 'T : equality> (displayListItem: 'T -> ReactElement) (model: Model<'T>) (dispatch: Message<'T> -> unit) =
        let renderItem item =
            let isSelected = model.SelectedItems |> List.contains item
            let style = [
                Bootstrap.listGroupItem
                Bootstrap.listGroupItemAction
                "pointer"
            ]
            let style' = if isSelected then Bootstrap.active::style else style
            a [ 
                classes style'
                OnClick (fun _ -> ToggleItemSelection item |> dispatch)
            ] [
                displayListItem item
            ]

        div [ Class Bootstrap.listGroup ] [
            yield! model.AllItems |> List.map renderItem
        ]

    let render<'T when 'T : equality> (props: SelectionListProps<'T>, key: string) =
        let init = init<'T> props
        let update = update<'T> props.OnSelectionChanged
        let view = view<'T> props.DisplayListItem

        React.elmishComponent ("SelectionModal", init, update, view, key)