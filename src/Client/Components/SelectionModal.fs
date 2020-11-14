namespace Client.Components

open System
open Fable
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Feliz
open Feliz.ElmishComponents

open Client.ClientStyle
open Client.ClientStyle.Helpers

module SelectionList =
    type Model<'T> = {
        AllItems: 'T list
        SelectionMode: SelectionMode
        SelectedItems: 'T list
        Filter: string
        OnSelectionChanged: 'T list -> unit
        ListItemToString: 'T -> string
    }
    and SelectionListProps<'T> = 
        {|
            SelectionMode: SelectionMode
            AllItems: 'T list
            SelectedItems: 'T list
            OnSelectionChanged: 'T list -> unit
            ListItemToString: 'T -> string
        |}
    and SelectionMode =
        | SingleSelect
        | MultiSelect

    type Message<'T> =
        | FilterChanged of string
        | ToggleItemSelection of 'T


    let render<'T when 'T : equality> (props: SelectionListProps<'T>, key) =
        let init (props: SelectionListProps<'T>) =
            { 
                SelectionMode = props.SelectionMode
                AllItems = props.AllItems
                SelectedItems = props.SelectedItems
                Filter = ""
                OnSelectionChanged = props.OnSelectionChanged
                ListItemToString = props.ListItemToString
            }, Cmd.none

        let update (msg: Message<'T>) (model: Model<'T>): Model<'T> * Cmd<Message<'T>> = 
            match msg with
            | FilterChanged newFilter ->
                { model with Filter = newFilter }, Cmd.none
            | ToggleItemSelection item ->
                let selectedItems =
                    match model.SelectionMode with
                    | SingleSelect ->
                        [ item ]
                    | MultiSelect ->
                        if model.SelectedItems |> List.contains item 
                        then model.SelectedItems |> List.filter ((<>) item)
                        else item::model.SelectedItems

                model.OnSelectionChanged(selectedItems)
                { model with SelectedItems = selectedItems }, Cmd.none

        let view (model: Model<'T>) (dispatch: Message<'T> -> unit) =
            let filteredItems =
                match model.Filter with
                | x when String.IsNullOrWhiteSpace(x) -> model.AllItems
                | x -> model.AllItems |> List.filter (fun item -> (model.ListItemToString item).ToLowerInvariant().Contains(x.ToLowerInvariant()))

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
                    str (model.ListItemToString item)
                ]

            [
                div [ classes [ Bootstrap.inputGroup; Bootstrap.inputGroupSm; Bootstrap.mb2 ]; Style [ MaxWidth "250px" ] ] [ 
                    input [ Type "text"
                            Class Bootstrap.formControl
                            Placeholder ""
                            OnChange (fun e -> FilterChanged e.Value |> dispatch) ]
                    div [ Class Bootstrap.inputGroupAppend ] [
                        span [ Class Bootstrap.inputGroupText ] [
                            i [ classes [ FontAwesome.fa; FontAwesome.faSearch ] ] [ ] 
                        ]
                    ]
                ]
                div [ Class Bootstrap.listGroup ] [
                    yield! filteredItems |> List.map renderItem
                ]
            ]
            |> fragment []

        React.elmishComponent ("SelectionModal", init props, update, view, key)