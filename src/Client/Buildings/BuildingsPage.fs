module Client.Buildings.BuildingsPage

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz

open Shared.Read
open Shared.Remoting
open Client
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.SortableTable

type SortableAttribute =
    | Code
    | Name
    | OrganizationNumber
    | Street
    | ZipCode
    | Town
    member me.ToString' () =
        match me with
        | Code -> "Code"
        | Name -> "Naam"
        | OrganizationNumber -> "OndernemingsNr"
        | Street -> "Straat"
        | ZipCode -> "Postcode"
        | Town -> "Plaats"
    member me.StringValueOf': BuildingListItem -> string =
        match me with
        | Name -> (fun li -> li.Name)
        | Code -> (fun li -> li.Code)
        | OrganizationNumber -> (fun li -> defaultArg li.OrganizationNumber "")
        | Street -> (fun li -> defaultArg li.Address.Street "")
        | ZipCode -> (fun li -> defaultArg li.Address.ZipCode "")
        | Town -> (fun li -> defaultArg li.Address.Town "")
    member me.Compare': BuildingListItem -> BuildingListItem -> int =
        fun li otherLi ->
            let result = me.StringValueOf'(li).CompareTo(me.StringValueOf'(otherLi))
            result
    static member All = [ Code; Name; OrganizationNumber; Street; ZipCode; Town ]
    interface ISortableAttribute<BuildingListItem> with
        member me.ToString = me.ToString'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi

type State = {
    CurrentUser: CurrentUser
    SelectedListItems: BuildingListItem list
    SelectedTab: Tab
    CurrentBuildingId: Guid option
    LoadingListItems: bool
    ListItems: BuildingListItem list
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
    | RemoveListItem of BuildingListItem
    | ListItemRemoved of Result<BuildingListItem, AuthorizationError>
    | Created of Building
    | Edited of Building
    | CurrentBuildingChanged of BuildingListItem

type BuildingsPageProps = {|
    CurrentUser: CurrentUser
    BuildingId: Guid option
    CurrentBuildingId: Guid option
|}

let init (props: BuildingsPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        SelectedListItems = []
        SelectedTab = List
        ListItems = []
        LoadingListItems = true
        CurrentBuildingId = props.CurrentBuildingId
    }
    
    let cmd =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetBuildings)
            ()
            (fun buildings -> Loaded (buildings, props.BuildingId))
            RemotingError
    state, cmd

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.BuildingId = listItem.BuildingId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.Code)
        { state with SelectedListItems = newlySelectedItems; SelectedTab = Details listItem }, Routing.navigateToPage (Routing.Page.BuildingDetails listItem.BuildingId)
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.BuildingId <> listItem.BuildingId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Cmd.none
    | SelectTab tab ->
        let cmd =
            match tab with
            | List -> Routing.navigateToPage (Routing.Page.BuildingList)
            | Details li -> Routing.navigateToPage (Routing.Page.BuildingDetails li.BuildingId)
            | New -> Routing.navigateToPage (Routing.Page.BuildingList)
        { state with SelectedTab = tab }, cmd
    | Loaded (buildings, selectedBuildingId) ->
        let newState = { state with ListItems = buildings |> List.sortBy (fun b -> b.Code); LoadingListItems = false }
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
    | RemotingError e ->
        //TODO.
        state, Cmd.none
    | Created building ->
        let listItem = building.ToListItem()
        let newListItems = listItem :: state.ListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        { state with 
            ListItems = newListItems
            SelectedListItems = newSelectedListItems
        }, (SelectTab (Tab.Details listItem)) |> Cmd.ofMsg
    | Edited building ->
        let listItem = building.ToListItem()
        let newListItems = state.ListItems |> List.map (fun li -> if li.BuildingId = building.BuildingId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.BuildingId = building.BuildingId then listItem else li)
        { state with 
            ListItems = newListItems
            SelectedListItems = newSelectedListItems
        }, (SelectTab (Tab.Details listItem)) |> Cmd.ofMsg
    | CurrentBuildingChanged building ->
        { state with CurrentBuildingId = Some building.BuildingId }, Cmd.none


let view (state: State) (dispatch: Msg -> unit): ReactElement =
    let determineNavItemStyle (tab: Tab) =
        let extraClasses =
            if state.SelectedTab = tab 
            then
                [ Bootstrap.active ]
            else
                []
        String.Join(" ", Bootstrap.navLink::extraClasses)

    div [ Class Bootstrap.row ] [
        div [ Class Bootstrap.colMd3 ] [
            div [ classes [ Bootstrap.nav; Bootstrap.navTabs; "left-tabs" ] ] [
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle List); OnClick (fun _ -> SelectTab List |> dispatch) ] 
                        [ str "Overview" ] 
                ]
                for selected in state.SelectedListItems do
                    yield li [ Class Bootstrap.navItem ] [
                        a 
                            [ Class (determineNavItemStyle (Details selected)); OnClick (fun _ -> SelectTab (Details selected) |> dispatch) ] 
                            [ str (sprintf "%s: %s" selected.Code selected.Name) ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuw gebouw" ]
                ]
            ]
        ]
              
        let list (state: State) =
            let currentBuildingId = state.CurrentBuildingId |> Option.defaultValue (Guid.Empty)

            SortableTable.render 
                {|
                    ListItems = state.ListItems
                    DisplayAttributes = SortableAttribute.All
                    IsSelected = Some (fun li -> li.BuildingId = currentBuildingId)
                    OnSelect = Some (CurrentBuildingChanged >> dispatch)
                    OnEdit = Some (AddDetailTab >> dispatch)
                    OnDelete = Some (RemoveListItem >> dispatch)
                    Key = "BuildingsPageTable"
                |}

        div [ Class Bootstrap.colMd9 ] [
            div [ Class Bootstrap.tabContent ] [
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
    ]