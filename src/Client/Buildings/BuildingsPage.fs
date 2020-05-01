module Client.Buildings.BuildingsPage

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Domain
open Client
open Client.ClientStyle

type SortableAttribute =
    | Code
    | Name
    | OrganizationNumber
    | Street
    | ZipCode
    | Town
    override me.ToString () =
        match me with
        | Code -> "Code"
        | Name -> "Naam"
        | OrganizationNumber -> "OndernemingsNr"
        | Street -> "Straat"
        | ZipCode -> "Postcode"
        | Town -> "Plaats"
    member me.ValueOf: BuildingListItem -> string =
        match me with
        | Name -> (fun li -> li.Name)
        | Code -> (fun li -> li.Code)
        | OrganizationNumber -> (fun li -> li.OrganizationNumber |> Option.defaultValue "")
        | Street -> (fun li -> li.Address.Street)
        | ZipCode -> (fun li -> li.Address.ZipCode)
        | Town -> (fun li -> li.Address.Town)

let listOfAttributes = [ 
    Code
    Name
    OrganizationNumber
    Street
    ZipCode
    Town 
]

type State = {
    CurrentUser: CurrentUser
    SelectedListItems: BuildingListItem list
    SelectedTab: Tab
    LoadingListItems: bool
    ListItems: BuildingListItem list
    SortOn: (SortableAttribute * bool) list
}
and Tab =
    | List
    | Details of BuildingListItem
    | New
type Msg =
    | AddDetailTab of BuildingListItem
    | RemoveDetailTab of BuildingListItem
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: BuildingListItem list * selectedListItemId: Guid option
    | AddSortOn of SortableAttribute
    | SortOn of SortableAttribute
    | RemoveListItem of BuildingListItem
    | ListItemRemoved of Result<BuildingListItem, InvariantError>
    | Created of Building
    | Edited of Building

type BuildingsPageProps = {|
    CurrentUser: CurrentUser
    BuildingId: Guid option
|}

let init (props: BuildingsPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        SelectedListItems = []
        SelectedTab = List
        ListItems = []
        LoadingListItems = true
        SortOn = []
    }
    
    let cmd =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetBuildings)
            ()
            (fun buildings -> Loaded (buildings, props.BuildingId))
            RemotingError
    state, cmd

let private performSort (sortOn: (SortableAttribute * bool) list) (listItems: BuildingListItem list) =
    let sortOnFunctions = 
        sortOn 
        |> List.rev 
        |> List.map (fun (attr, reverse) -> if reverse then List.sortByDescending (attr.ValueOf) else List.sortBy (attr.ValueOf))
    sortOnFunctions
    |> List.fold (fun acc nextSort -> nextSort acc) listItems

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let toListItem (building: Building): BuildingListItem = {
        BuildingId = building.BuildingId
        IsActive = building.IsActive
        Code = building.Code
        Name = building.Name
        Address = building.Address
        OrganizationNumber = building.OrganizationNumber
    }

    match msg with
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.BuildingId = listItem.BuildingId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.Code)
        { state with SelectedListItems = newlySelectedItems; SelectedTab = Details listItem }, Cmd.none
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.BuildingId <> listItem.BuildingId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Cmd.none
    | SelectTab tab ->
        { state with SelectedTab = tab }, Cmd.none
    | Loaded (buildings, selectedBuildingId) ->
        let newState = { state with ListItems = buildings; LoadingListItems = false }
        let cmd =
            match selectedBuildingId with
            | Some selectedBuildingId ->
                let selectedListItem = buildings |> List.tryFind (fun building -> building.BuildingId = selectedBuildingId)
                match selectedListItem with
                | Some selected -> AddDetailTab selected |> Cmd.ofMsg
                | None -> Cmd.none
            | None -> Cmd.none
        newState, cmd
    | RemoveListItem building ->
        let cmd =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi().DeleteBuilding)
                building.BuildingId
                (fun r -> r |> Result.map (fun _ -> building) |> ListItemRemoved)
                RemotingError

        let newSelection =
            state.SelectedListItems |> List.filter (fun selected -> selected.BuildingId <> building.BuildingId)

        let newItems =
            state.ListItems |> List.filter (fun item -> item.BuildingId <> building.BuildingId)

        { state with SelectedListItems = newSelection; ListItems = newItems }, cmd
    | ListItemRemoved result ->
        match result with
        | Ok _ -> state, Cmd.none
        | Error e -> //TODO...
            state, Cmd.none
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
    | RemotingError e ->
        //TODO.
        state, Cmd.none
    | Created building ->
        let listItem = toListItem building
        let newListItems = listItem :: state.ListItems |> performSort state.SortOn
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems }, Cmd.none
    | Edited building ->
        let listItem = toListItem building
        let newListItems = state.ListItems |> List.map (fun li -> if li.BuildingId = building.BuildingId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.BuildingId = building.BuildingId then listItem else li)
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems }, Cmd.none

let view (state: State) (dispatch: Msg -> unit): ReactElement =
    let determineNavItemStyle (tab: Tab) =
        let extraClasses =
            if state.SelectedTab = tab 
            then
                [ MeliorStyle.active ]
            else
                []
        String.Join(" ", MeliorStyle.navLink::extraClasses)

    div [ Class MeliorStyle.row ] [
        div [ Class MeliorStyle.col3 ] [
            div [ Class (sprintf "%s %s %s" MeliorStyle.nav MeliorStyle.flexColumn MeliorStyle.navPills) ] [
                yield li [ Class MeliorStyle.navItem ] [
                    a 
                        [ Class (determineNavItemStyle List); OnClick (fun _ -> SelectTab List |> dispatch) ] 
                        [ str "Overview" ] 
                ]
                for selected in state.SelectedListItems do
                    yield li [ Class MeliorStyle.navItem ] [
                        a 
                            [ Class (determineNavItemStyle (Details selected)); OnClick (fun _ -> SelectTab (Details selected) |> dispatch) ] 
                            [ str (sprintf "%s: %s" selected.Code selected.Name) ]
                    ]
                yield li [ Class MeliorStyle.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuw gebouw" ]
                ]
            ]
        ]
        
        let dispatchSortOn (attribute: SortableAttribute) (e: Browser.Types.MouseEvent) =
            if e.ctrlKey then
                AddSortOn attribute |> dispatch
            else
                SortOn attribute |> dispatch
        
        let sortingIndexNumber (attribute: SortableAttribute) =
            state.SortOn 
            |> List.tryFindIndex (fun (selected, _) -> selected = attribute)
            |> Option.map (fun index -> sprintf " (%s)" (string (index + 1)))
            |> Option.defaultValue ""
        
        let header (attribute: SortableAttribute) =
            th 
                [ OnClick (dispatchSortOn attribute) ]
                [ str (sprintf "%s%s" (string attribute) (sortingIndexNumber attribute)) ]
        
        let list state =
            table [ Class (sprintf "%s %s %s" MeliorStyle.table MeliorStyle.tableStriped MeliorStyle.tableHover) ] [
                thead [] [
                    tr [] [
                        yield! (listOfAttributes |> List.map header)
                        th [] []
                        th [] []
                    ]
                ]
                tbody []
                    (state.ListItems
                    |> List.map (fun li -> 
                        tr [] [
                            yield! (listOfAttributes |> List.map (fun attr -> td [] [ str (attr.ValueOf li) ]))
                            yield
                                td [] [ 
                                    button 
                                        [ OnClick (fun _ -> AddDetailTab li |> dispatch) ] 
                                        [ str "Openen" ] 
                                ]
                            yield
                                td [] [ 
                                    button 
                                        [ OnClick (fun _ -> RemoveListItem li |> dispatch) ] 
                                        [ str "Verwijderen" ] 
                                ]
                        ]
                    ))
            ]

        div [ Class MeliorStyle.col ] [
            match state.SelectedTab with
            | List -> list state
            | Details listItem -> 
                BuildingDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        Identifier = listItem.BuildingId
                        IsNew = false
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
            | New ->
                BuildingDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        Identifier = Guid.NewGuid()
                        IsNew = true
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
        ]
    ]

let render (props: BuildingsPageProps) =
    React.elmishComponent ("BuildingsPage", init props, update, view)