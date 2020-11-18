module Client.SortableTable

open System
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
    abstract member ReactElementFor: ('T -> ReactElement)
    abstract member ExtraHeaderAttributes: IHTMLProp seq
    abstract member StringValueOf: ('T -> string)
    abstract member Compare: 'T -> 'T -> int
    abstract member IsFilterable: bool

[<NoComparison; NoEquality>]
type SortableTableProps<'T, 'U when 'U :> ISortableAttribute<'T>> =
    {|
        ListItems: 'T list
        DisplayAttributes: 'U list
        Key: string
        IsSelected: ('T -> bool) option
        OnSelect: ('T -> unit) option
        IsEditable: ('T -> bool) option
        OnEdit: ('T -> unit) option
        IsDeletable: ('T -> bool) option
        OnDelete: ('T -> unit) option
    |}

type State<'T, 'U when 'U :> ISortableAttribute<'T>> = {
    AllListItems: 'T list
    FilteredListItems: 'T list
    DisplayAttributes: 'U list
    SortOn: (ISortableAttribute<'T> * bool) list
    FilterOn: (ISortableAttribute<'T> * string) list
    IsSelected: ('T -> bool) option
    OnSelect: ('T -> unit) option
    IsEditable: ('T -> bool) option
    OnEdit: ('T -> unit) option
    IsDeletable: ('T -> bool) option
    OnDelete: ('T -> unit) option
}

type Msg<'T> =
    | AddSortOn of ISortableAttribute<'T>
    | SortOn of ISortableAttribute<'T>
    | SetFilterOn of ISortableAttribute<'T> * string

let init (props: SortableTableProps<'T, 'U>): State<'T, 'U> * Cmd<Msg<'T>> =
    {
        AllListItems = props.ListItems
        FilteredListItems = props.ListItems
        DisplayAttributes = props.DisplayAttributes
        SortOn = []
        FilterOn = []
        IsSelected = props.IsSelected
        OnSelect = props.OnSelect
        IsDeletable = props.IsDeletable
        OnDelete = props.OnDelete
        IsEditable = props.IsEditable
        OnEdit = props.OnEdit
    }, Cmd.none

let [<Global>] console: JS.Console = jsNative

let private performSort (sortOn: (ISortableAttribute<'T> * bool) list) (listItems: 'T list) =
    let sortOnFunctions = 
        sortOn 
        |> List.rev 
        |> List.map (fun (attr, reverse) -> 
            if reverse 
            then List.sortWith (fun li otherLi -> attr.Compare otherLi li)
            else List.sortWith (fun li otherLi -> attr.Compare li otherLi)
        )
    sortOnFunctions
    |> List.fold (fun acc nextSort -> nextSort acc) listItems

let private performFilter (filterOn: (ISortableAttribute<'T> * string) list) (listItems: 'T list) =
    match filterOn with
    | [] ->
        listItems
    | filterOn ->
        let predicates =
            filterOn
            |> List.map (fun (attr, filter) ->
                let filter = filter.ToLowerInvariant()
                fun li -> (attr.StringValueOf li).ToLowerInvariant().Contains(filter))

        listItems
        |> List.filter (fun li -> predicates |> List.forall (fun predicate -> predicate li))

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

        let newListItems = state.FilteredListItems |> performSort newSortOn
        { state with SortOn = newSortOn; FilteredListItems = newListItems }, Cmd.none
    | AddSortOn attribute ->
        let newSortOn =
            match state.SortOn |> List.tryFind (fun (attr, _) -> attr = attribute) with
            | Some (_, false) -> state.SortOn |> List.map (fun (attr, reverse) -> if attr = attribute then (attr, true) else (attr, reverse))
            | Some (_, true)  -> state.SortOn |> List.filter (fun (attr, _) -> attr <> attribute)
            | None            -> [ (attribute, false) ] |> List.append state.SortOn

        let newListItems = state.FilteredListItems |> performSort newSortOn
        { state with SortOn = newSortOn; FilteredListItems = newListItems }, Cmd.none
    | SetFilterOn (attribute, searchFilter) ->
        let newFilterOn =
            let filterOn' = state.FilterOn |> List.filter (fun (attr, _) -> attr <> attribute)
            if String.IsNullOrWhiteSpace searchFilter then
                filterOn'
            else
                (attribute, searchFilter)::filterOn'


        let newListItems = state.AllListItems |> performFilter newFilterOn |> performSort state.SortOn
        { state with 
            FilterOn = newFilterOn
            FilteredListItems = newListItems }, Cmd.none

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

    let sortingDirection (attribute: ISortableAttribute<'T>) =
        state.SortOn 
        |> List.tryPick (fun (attr, reversed) -> if attr = attribute then Some reversed else None)

    let selectedColumn, selectHeader =
        if state.OnSelect.IsSome && state.IsSelected.IsSome then
            fun li ->
                td [] [
                    div [ Class Bootstrap.formCheck ] [
                        button [
                            Type "button"
                            classes [ Bootstrap.btn; Bootstrap.btnSm; if state.IsSelected.Value (li) then Bootstrap.btnPrimary else Bootstrap.btnOutlinePrimary ]
                            OnClick (fun e -> e.preventDefault(); e.stopPropagation(); state.OnSelect.Value li)
                        ] [
                            str (if state.IsSelected.Value (li) then "Geselecteerd" else "Selecteren")
                        ]
                    ]
                ]
            , th [ classes [ Bootstrap.borderTop0; Bootstrap.borderBottom0 ] ] [] 
        else
            fun _ -> null
            , null

    let deleteColumn, deleteHeader =
        if state.OnDelete.IsSome then
            fun li ->
                td [ Style [ Width "30px" ] ] [
                    if (not state.IsDeletable.IsSome || state.IsDeletable.Value(li)) then
                        button [
                            classes [ Bootstrap.btn; Bootstrap.btnSm; Bootstrap.btnOutlineDanger ]
                            OnClick (fun e -> e.preventDefault(); e.stopPropagation(); state.OnDelete.Value (li)) 
                        ] [
                            str "Verwijderen"
                        ]
                    else
                        null
                ]
            , th [ classes [ Bootstrap.borderTop0; Bootstrap.borderBottom0 ]; Style [ Width "30px" ] ] []
        else
            fun _ -> null
            , null

    let header (attr: ISortableAttribute<'T>) =
        let name = attr.ToString ()
        th 
            [ yield classes [ Bootstrap.borderTop0; Bootstrap.borderBottom0 ]; yield! attr.ExtraHeaderAttributes ]
            [
                div [ OnClick (dispatchSortOn attr); Class "pointer" ] [
                    yield str (sprintf "%s%s " name (sortingIndexNumber attr)) 
                    yield 
                        match sortingDirection attr with
                        | Some true -> i [ classes [ FontAwesome.fa; FontAwesome.faSortDown ] ] []
                        | Some false -> i [ classes [ FontAwesome.fa; FontAwesome.faSortUp ] ] []
                        | None -> null
                ]
                div [ classes [ Bootstrap.inputGroup; Bootstrap.inputGroupSm ] ] [ 
                    input [ Type "text"
                            Class Bootstrap.formControl
                            Placeholder ""
                            OnChange (fun e -> SetFilterOn (attr, e.Value) |> dispatch) ]
                    div [ Class Bootstrap.inputGroupAppend ] [
                        span [ Class Bootstrap.inputGroupText ] [
                            i [ classes [ FontAwesome.fa; FontAwesome.faSearch ] ] [ ] 
                        ]
                    ]
                ]
            ]

    table [ classes [ Bootstrap.table; Bootstrap.tableStriped; Bootstrap.tableHover ] ] [
        thead [] [
            tr [] [
                yield! state.DisplayAttributes |> List.map (fun attr -> header attr)
                yield! [ selectHeader; deleteHeader ]
            ]
        ]
        tbody []
            (state.FilteredListItems
            |> List.map (fun li -> 
                tr [
                    if state.OnEdit.IsSome 
                    then yield! [ OnClick (fun _ -> state.OnEdit.Value li) :> IHTMLProp; Style [ Cursor "pointer" ] :> IHTMLProp ]
                    elif state.OnSelect.IsSome
                    then yield! [ OnClick (fun _ -> state.OnSelect.Value li) :> IHTMLProp; Style [ Cursor "pointer" ] :> IHTMLProp ]
                    
                    if (state.IsSelected.IsSome && state.IsSelected.Value li) 
                    then yield (Class Bootstrap.tableActive)
                ] [
                    yield! (state.DisplayAttributes |> List.map (fun attr -> td [] [ attr.ReactElementFor li ]))
                    yield! [ selectedColumn li; deleteColumn li ]
                ]
            ))
    ]


let render<'T, 'U when 'U :> ISortableAttribute<'T>> =
    FunctionComponent.Of (
        (fun (props: SortableTableProps<'T, 'U>) -> React.elmishComponent("SortableTable", init props, update, view, props.Key)),
        displayName = "SortableTableView",
        memoizeWith = memoEqualsButFunctions
    )