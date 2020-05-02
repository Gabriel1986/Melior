module Client.Residents.ResidentsPage

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
open Client.Components.SimpleResidentComponent

type State = {
    CurrentUser: CurrentUser
    SelectedListItems: ResidentListItem list
    SelectedTab: Tab
    LoadingListItems: bool
    ListItems: ResidentListItem list
}
and Tab =
    | List
    | Details of ResidentListItem
    | New
type Msg =
    | AddDetailTab of ResidentListItem
    | RemoveDetailTab of ResidentListItem
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: ResidentListItem list * selectedListItemId: Guid option
    | RemoveListItem of ResidentListItem
    | ListItemRemoved of Result<ResidentListItem, InvariantError>
    | Created of Resident
    | Edited of Resident

type LotsPageProps = {|
    CurrentUser: CurrentUser
    ResidentId: Guid option
|}

let init (props: LotsPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        SelectedListItems = []
        SelectedTab = List
        ListItems = []
        LoadingListItems = true
    }
    
    let cmd =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetResidents)
            ()
            (fun residents -> Loaded (residents, props.ResidentId))
            RemotingError
    state, cmd

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let toListItem (resident: Resident): ResidentListItem = {
        ResidentId = resident.ResidentId
        BuildingId = resident.BuildingId
        FirstName = resident.Person.FirstName
        LastName = resident.Person.LastName
        IsActive = resident.IsActive
        MovedInDate = resident.MovedInDate
        MovedOutDate = resident.MovedOutDate
    }

    match msg with
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.ResidentId = listItem.ResidentId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.LastName)
            |> List.sortBy (fun li -> li.FirstName)
        { state with SelectedListItems = newlySelectedItems; SelectedTab = Details listItem }, Routing.navigateToPage (Routing.Page.ResidentDetails listItem.ResidentId)
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.ResidentId <> listItem.ResidentId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Cmd.none
    | SelectTab tab ->
        { state with SelectedTab = tab }, Cmd.none
    | Loaded (residents, selectedResidentId) ->
        let newState = { state with ListItems = residents; LoadingListItems = false }
        let cmd =
            match selectedResidentId with
            | Some selectedResidentId ->
                let selectedListItem = residents |> List.tryFind (fun listItem -> listItem.ResidentId = selectedResidentId)
                match selectedListItem with
                | Some selected -> AddDetailTab selected |> Cmd.ofMsg
                | None -> Cmd.none
            | None -> Cmd.none
        newState, cmd
    | RemoveListItem building ->
        let cmd =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi().DeleteLot)
                building.ResidentId
                (fun r -> r |> Result.map (fun _ -> building) |> ListItemRemoved)
                RemotingError

        let newSelection =
            state.SelectedListItems |> List.filter (fun selected -> selected.ResidentId <> building.ResidentId)

        let newItems =
            state.ListItems |> List.filter (fun item -> item.ResidentId <> building.ResidentId)

        { state with SelectedListItems = newSelection; ListItems = newItems }, cmd
    | ListItemRemoved result ->
        match result with
        | Ok _ -> state, Cmd.none
        | Error e -> //TODO...
            state, Cmd.none
    | RemotingError e ->
        //TODO.
        state, Cmd.none
    | Created resident ->
        let listItem = toListItem resident
        let newListItems = listItem :: state.ListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems }, Cmd.none
    | Edited resident ->
        let listItem = toListItem resident
        let newListItems = state.ListItems |> List.map (fun li -> if li.ResidentId = resident.ResidentId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.ResidentId = resident.ResidentId then listItem else li)
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems }, Cmd.none

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
        div [ Class Bootstrap.col3 ] [
            div [ Class (sprintf "%s %s %s" Bootstrap.nav Bootstrap.flexColumn Bootstrap.navPills) ] [
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle List); OnClick (fun _ -> SelectTab List |> dispatch) ] 
                        [ str "Overview" ] 
                ]
                for selected in state.SelectedListItems do
                    yield li [ Class Bootstrap.navItem ] [
                        a 
                            [ Class (determineNavItemStyle (Details selected)); OnClick (fun _ -> SelectTab (Details selected) |> dispatch) ] 
                            [ str (sprintf "%s %s" selected.LastName selected.FirstName) ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe bewoner" ]
                ]
            ]
        ]
              
        let list (state: State) =
            SortableTable.render 
                {|
                    ListItems = state.ListItems
                    DisplayAttributes = SortableResidentListItemAttribute.All
                    ExtraColumnHeaders = [ 
                        th [] []; 
                        th [] [] 
                    ]
                    ExtraColumns = (fun li -> 
                        seq {
                            td [] [ 
                                button 
                                    [ 
                                        Class (sprintf "%s %s" Bootstrap.btn Bootstrap.btnLink)
                                        OnClick (fun _ -> AddDetailTab li |> dispatch) 
                                    ] 
                                    [
                                        i [ Class (sprintf "%s %s" FontAwesome.fa FontAwesome.faExternalLinkAlt) ] []
                                    ] 
                            ]
                            td [] [ 
                                button 
                                    [
                                        Class (sprintf "%s %s" Bootstrap.btn Bootstrap.btnLink)
                                        OnClick (fun _ -> RemoveListItem li |> dispatch) 
                                    ] 
                                    [
                                        i [ Class (sprintf "%s %s" FontAwesome.far FontAwesome.faTrashAlt) ] []
                                    ] 
                            ]
                        }
                    )
                    Key = "BuildingsPageTable"
                |}

        div [ Class Bootstrap.col ] [
            match state.SelectedTab with
            | List -> list state
            | Details listItem -> 
                ResidentDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        Identifier = listItem.ResidentId
                        IsNew = false
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
            | New ->
                ResidentDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        Identifier = Guid.NewGuid()
                        IsNew = true
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
        ]
    ]

let render (props: LotsPageProps) =
    React.elmishComponent ("LotsPage", init props, update, view)