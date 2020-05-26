module Client.SortableTable

open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Client.ClientStyle
open Client.ClientStyle.Helpers
open Fable.Core

type ISortableAttribute<'T> =
    abstract member ToString: (unit -> string)
    abstract member StringValueOf: ('T -> string)
    abstract member Compare: 'T -> 'T -> int

[<NoComparison; NoEquality>]
type SortableTableProps<'T, 'U when 'U :> ISortableAttribute<'T>> =
    {|
        ListItems: 'T list
        DisplayAttributes: 'U list
        ExtraColumnHeaders: ReactElement seq
        ExtraColumns: 'T -> ReactElement seq
        Key: string
    |}

type State<'T, 'U when 'U :> ISortableAttribute<'T>> = {
    ListItems: 'T list
    DisplayAttributes: 'U list
    ExtraColumnHeaders: ReactElement seq
    ExtraColumns: 'T -> ReactElement seq
    SortOn: (ISortableAttribute<'T> * bool) list
}

type Msg<'T> =
    | AddSortOn of ISortableAttribute<'T>
    | SortOn of ISortableAttribute<'T>



let init (props: SortableTableProps<'T, 'U>): State<'T, 'U> * Cmd<Msg<'T>> =
    {
        ListItems = props.ListItems
        DisplayAttributes = props.DisplayAttributes
        ExtraColumnHeaders = props.ExtraColumnHeaders
        ExtraColumns = props.ExtraColumns
        SortOn = []
    }, Cmd.none

let [<Global>] console: JS.Console = jsNative

let private performSort (sortOn: (ISortableAttribute<'T> * bool) list) (listItems: 'T list) =
    let sortOnFunctions = 
        sortOn 
        |> List.rev 
        |> List.map (fun (attr, reverse) -> 
            let compare = attr.Compare
            if reverse 
            then List.sortWith (fun li otherLi -> attr.Compare otherLi li)
            else List.sortWith (fun li otherLi -> attr.Compare li otherLi)
        )
    sortOnFunctions
    |> List.fold (fun acc nextSort -> nextSort acc) listItems

let update (msg: Msg<'T>) (state: State<'T, 'U>): State<'T, 'U> * Cmd<Msg<'T>> =
    match msg with
    | SortOn attribute ->
        let newSortOn =
            if state.SortOn |> List.length = 1 && fst state.SortOn.Head = attribute
            then
                let attr, reverse = state.SortOn.Head
                if not reverse then [ (attr, true) ]
                else []
            else
                [ (attribute, false) ]

        let newListItems = state.ListItems |> performSort newSortOn
        { state with SortOn = newSortOn; ListItems = newListItems }, Cmd.none
    | AddSortOn attribute ->
        let newSortOn =
            match state.SortOn |> List.tryFind (fun (attr, _) -> attr = attribute) with
            | Some (_, false) -> state.SortOn |> List.map (fun (attr, reverse) -> if attr = attribute then (attr, true) else (attr, reverse))
            | Some (_, true)  -> state.SortOn |> List.filter (fun (attr, _) -> attr <> attribute)
            | None            -> [ (attribute, false) ] |> List.append state.SortOn

        let newListItems = state.ListItems |> performSort newSortOn
        { state with SortOn = newSortOn; ListItems = newListItems }, Cmd.none

let view (state: State<'T, 'U>) (dispatch: Msg<'T> -> unit) =
    let dispatchSortOn (attribute: ISortableAttribute<'T>) (e: Browser.Types.MouseEvent) =
        if e.ctrlKey then
            AddSortOn attribute |> dispatch
        else
            SortOn attribute |> dispatch

    let sortingIndexNumber (attribute: ISortableAttribute<'T>) =
        state.SortOn 
        |> List.tryFindIndex (fun (selected, _) -> selected = attribute)
        |> Option.map (fun index -> sprintf " (%s)" (string (index + 1)))
        |> Option.defaultValue ""

    let header (attr: ISortableAttribute<'T>) =
        th 
            [ OnClick (dispatchSortOn attr) ]
            [ str (sprintf "%s%s" (attr.ToString ()) (sortingIndexNumber attr)) ]

    table [ classes [ Bootstrap.table; Bootstrap.tableStriped; Bootstrap.tableHover ] ] [
        thead [] [
            tr [] [
                yield! state.DisplayAttributes |> List.map (fun attr -> header attr)
                yield! state.ExtraColumnHeaders
            ]
        ]
        tbody []
            (state.ListItems
            |> List.map (fun li -> 
                tr [] [
                    yield! (state.DisplayAttributes |> List.map (fun attr -> td [] [ str (attr.StringValueOf li) ]))
                    yield! (state.ExtraColumns li)
                ]
            ))
    ]


let render<'T, 'U when 'U :> ISortableAttribute<'T>> (props: SortableTableProps<'T, 'U>) =
    React.elmishComponent("SortableTable", init props, update, view, props.Key)