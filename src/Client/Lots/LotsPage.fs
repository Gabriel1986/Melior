﻿module Client.Lots.LotsPage

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Domain
open Shared.Remoting
open Client
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.SortableTable

type State = {
    CurrentUser: CurrentUser
    CurrentBuilding: BuildingListItem
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
    | ListItemRemoved of Result<LotListItem, AuthorizationError>
    | Created of Lot
    | Edited of Lot

type LotsPageProps = {|
    CurrentUser: CurrentUser
    CurrentBuilding: BuildingListItem
    LotId: Guid option
|}

type SortableLotListItemAttribute =
    | BuildingCode
    | OwnerName
    | Code
    | LotType
    | Floor
    | Description
    | IsActive
    member me.ToString' () =
        match me with
        | BuildingCode -> "Gebouw"
        | OwnerName -> "Eigenaar"
        | Code -> "Code"
        | LotType -> "Type"
        | Floor -> "Verdieping"
        | Description -> "Omschrijving"
        | IsActive -> "Actief"
    member me.StringValueOf': LotListItem -> string =
        match me with
        | BuildingCode -> (fun li -> li.Building.Name)
        | OwnerName -> (fun li -> match li.CurrentOwner with | Person p -> sprintf "%s %s" p.LastName p.FirstName | Organization o -> o.Name)
        | Code -> (fun li -> li.Code)
        | LotType -> (fun li -> string li.LotType)
        | Floor -> (fun li -> string li.Floor)
        | Description -> (fun li -> li.Description |> Option.defaultValue "")
        | IsActive -> (fun li -> if li.IsActive then "Ja" else "Nee")
    member me.Compare': LotListItem -> LotListItem -> int =
        match me with
        | Floor -> 
            fun li otherLi -> li.Floor - otherLi.Floor
        | IsActive -> 
            fun li otherLi ->
                if li.IsActive = otherLi.IsActive then 0 
                elif li.IsActive && not otherLi.IsActive then 1 else -1
        | _     -> 
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ BuildingCode; Code;  LotType; Floor; Description; IsActive ]
    interface ISortableAttribute<LotListItem> with
        member me.ToString = me.ToString'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi

let init (props: LotsPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        CurrentBuilding = props.CurrentBuilding
        SelectedListItems = []
        SelectedTab = List
        ListItems = []
        LoadingListItems = true
    }
    
    let cmd =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetLots)
            {| BuildingId = props.CurrentBuilding.BuildingId |}
            (fun lots -> Loaded (lots, props.LotId))
            RemotingError
    state, cmd

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let toListItem (lot: Lot): LotListItem = {
        LotId = lot.LotId
        Code = lot.Code
        Building = lot.Building
        CurrentOwner = lot.CurrentOwner
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
        { state with SelectedListItems = newlySelectedItems; SelectedTab = Details listItem }, Routing.navigateToPage (Routing.Page.LotDetails { BuildingId = state.CurrentBuilding.BuildingId; DetailId = listItem.LotId })
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
    | RemoveListItem lot ->
        let cmd =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi().DeleteLot)
                lot.LotId
                (fun r -> r |> Result.map (fun _ -> lot) |> ListItemRemoved)
                RemotingError

        let newSelection =
            state.SelectedListItems |> List.filter (fun selected -> selected.LotId <> lot.LotId)

        let newItems =
            state.ListItems |> List.filter (fun item -> item.LotId <> lot.LotId)

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
        div [ Class Bootstrap.colMd3 ] [
            div [ classes [ Bootstrap.nav; Bootstrap.flexMdColumn; Bootstrap.navPills ] ] [
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle List); OnClick (fun _ -> SelectTab List |> dispatch) ] 
                        [ str "Overview" ] 
                ]
                for selected in state.SelectedListItems do
                    yield li [ Class Bootstrap.navItem ] [
                        a 
                            [ Class (determineNavItemStyle (Details selected)); OnClick (fun _ -> SelectTab (Details selected) |> dispatch) ] 
                            [ str selected.Code ]
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
                                a 
                                    [ 
                                        classes [ Bootstrap.textPrimary; "pointer" ]
                                        OnClick (fun _ -> AddDetailTab li |> dispatch) 
                                    ] 
                                    [
                                        i [ classes [ FontAwesome.fa; FontAwesome.faExternalLinkAlt ] ] []
                                    ] 
                            ]
                            td [] [ 
                                a 
                                    [
                                        classes [ Bootstrap.textDanger; "pointer" ]
                                        OnClick (fun _ -> RemoveListItem li |> dispatch) 
                                    ] 
                                    [
                                        i [ classes [ FontAwesome.far; FontAwesome.faTrashAlt ] ] []
                                    ] 
                            ]
                        }
                    )
                    Key = "LotsPageTable"
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