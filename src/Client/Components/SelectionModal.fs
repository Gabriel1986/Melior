namespace Client.Components

open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Client.ClientStyle
open Client.ClientStyle.Helpers

module SelectionList =
    type Model<'T> = {
        AllItems: 'T list
        SelectionMode: SelectionMode
        SelectedItems: 'T list
    }
    and SelectionListProps<'T> = 
        {|
            SelectionMode: SelectionMode
            AllItems: 'T list
            SelectedItems: 'T list
            DisplayListItem: 'T -> ReactElement
            OnSelectionChanged: 'T list -> unit
        |}
    and SelectionMode =
        | SingleSelect
        | MultiSelect

    type Message<'T> =
        | ToggleItemSelection of 'T


    let render<'T when 'T : equality> (props: SelectionListProps<'T>, key) =
        let init (props: SelectionListProps<'T>) =
            { 
                SelectionMode = props.SelectionMode
                AllItems = props.AllItems
                SelectedItems = props.SelectedItems
            }, Cmd.none

        let update (onSelectionChanged: 'T list -> unit) (msg: Message<'T>) (model: Model<'T>): Model<'T> * Cmd<Message<'T>> = 
            match msg with
            | ToggleItemSelection item ->
                let selectedItems =
                    match model.SelectionMode with
                    | SingleSelect ->
                        [ item ]
                    | MultiSelect ->
                        if model.SelectedItems |> List.contains item 
                        then model.SelectedItems |> List.filter ((<>) item)
                        else item::model.SelectedItems

                onSelectionChanged(selectedItems)
                { model with SelectedItems = selectedItems }, Cmd.none

        let view (displayListItem: 'T -> ReactElement) (model: Model<'T>) (dispatch: Message<'T> -> unit) =
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

        React.elmishComponent ("SelectionModal", init props, update props.OnSelectionChanged, view props.DisplayListItem, key)