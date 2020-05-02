module Client.Lots.LotsPage

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
open Client.Components.SimpleLotComponent

type State = {
    CurrentUser: CurrentUser
    SelectedListItems: LotListItem list
    SelectedTab: Tab
    LoadingListItems: bool
    ListItems: LotListItem list
}
and Tab =
    | List
    | Details of LotListItem
    | New
type Msg =
    | AddDetailTab of LotListItem
    | RemoveDetailTab of LotListItem
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: LotListItem list * selectedListItemId: Guid option
    | RemoveListItem of LotListItem
    | ListItemRemoved of Result<LotListItem, InvariantError>
    | Created of Lot
    | Edited of Lot

type LotsPageProps = {|
    CurrentUser: CurrentUser
    LotId: Guid option
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
            (Remoting.getRemotingApi().GetLots)
            ()
            (fun lots -> Loaded (lots, props.LotId))
            RemotingError
    state, cmd

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let toListItem (lot: Lot): LotListItem = {
        LotId = lot.LotId
        Code = lot.Code
        Building = lot.Building
        LotType = lot.LotType
        Floor = lot.Floor
        Description = lot.Description
        IsActive = lot.IsActive
    }

    match msg with
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.LotId = listItem.LotId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.Description)
            |> List.sortBy (fun li -> li.Floor)
        { state with SelectedListItems = newlySelectedItems; SelectedTab = Details listItem }, Routing.navigateToPage (Routing.Page.LotDetails listItem.LotId)
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.LotId <> listItem.LotId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Cmd.none
    | SelectTab tab ->
        { state with SelectedTab = tab }, Cmd.none
    | Loaded (lots, selectedLotId) ->
        let newState = { state with ListItems = lots; LoadingListItems = false }
        let cmd =
            match selectedLotId with
            | Some selectedLotId ->
                let selectedListItem = lots |> List.tryFind (fun listItem -> listItem.LotId = selectedLotId)
                match selectedListItem with
                | Some selected -> AddDetailTab selected |> Cmd.ofMsg
                | None -> Cmd.none
            | None -> Cmd.none
        newState, cmd
    | RemoveListItem building ->
        let cmd =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi().DeleteLot)
                building.LotId
                (fun r -> r |> Result.map (fun _ -> building) |> ListItemRemoved)
                RemotingError

        let newSelection =
            state.SelectedListItems |> List.filter (fun selected -> selected.LotId <> building.LotId)

        let newItems =
            state.ListItems |> List.filter (fun item -> item.LotId <> building.LotId)

        { state with SelectedListItems = newSelection; ListItems = newItems }, cmd
    | ListItemRemoved result ->
        match result with
        | Ok _ -> state, Cmd.none
        | Error e -> //TODO...
            state, Cmd.none
    | RemotingError e ->
        //TODO.
        state, Cmd.none
    | Created lot ->
        let listItem = toListItem lot
        let newListItems = listItem :: state.ListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems }, Cmd.none
    | Edited lot ->
        let listItem = toListItem lot
        let newListItems = state.ListItems |> List.map (fun li -> if li.LotId = lot.LotId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.LotId = lot.LotId then listItem else li)
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
                            [ str (sprintf "%s: %s" selected.Building.Code selected.Code) ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe kavel" ]
                ]
            ]
        ]
              
        let list (state: State) =
            SortableTable.render 
                {|
                    ListItems = state.ListItems
                    DisplayAttributes = SortableLotListItemAttribute.All
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
                LotDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        Identifier = listItem.LotId
                        IsNew = false
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
            | New ->
                LotDetails.render 
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