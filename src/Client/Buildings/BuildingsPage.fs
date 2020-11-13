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

open Client.Library

type SortableAttribute =
    | Code
    | Name
    | OrganizationNumber
    | Address
    override me.ToString () =
        match me with
        | Code -> "Code"
        | Name -> "Naam"
        | OrganizationNumber -> "Ond. nr."
        | Address -> "Adres"
    member me.ReactElementFor': BuildingListItem -> ReactElement =
        fun li -> str (me.StringValueOf' li) 
    member me.StringValueOf': BuildingListItem -> string =
        match me with
        | Name -> (fun li -> li.Name)
        | Code -> (fun li -> li.Code)
        | OrganizationNumber -> (fun li -> defaultArg li.OrganizationNumber "")
        | Address -> (fun li -> string li.Address)
    member me.Compare': BuildingListItem -> BuildingListItem -> int =
        fun li otherLi -> me.StringValueOf'(li).CompareTo(me.StringValueOf'(otherLi))
    static member All = [ Code; Name; OrganizationNumber; Address ]
    interface ISortableAttribute<BuildingListItem> with
        member me.ReactElementFor = me.ReactElementFor'
        member _.ExtraHeaderAttributes = Seq.empty
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true

type State = {
    CurrentUser: User
    SelectedListItems: BuildingListItem list
    SelectedTab: Tab
    CurrentBuildingId: Guid option
    LoadingListItems: bool
    ListItems: BuildingListItem list
}
and Tab =
    | List
    | Details of buildingId: Guid
    | New
type Msg =
    | AddDetailTab of BuildingListItem
    | RemoveDetailTab of BuildingListItem
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: BuildingListItem list * selectedListItemId: Guid option
    | RemoveListItem of BuildingListItem
    | ConfirmRemoveListItem of BuildingListItem
    | ListItemRemoved of Result<BuildingListItem, DeleteBuildingError>
    | Created of Building
    | Edited of Building
    | CurrentBuildingChanged of BuildingListItem
    | NoOp

type BuildingsPageProps = {|
    CurrentUser: User
    BuildingId: Guid option
    CurrentBuildingId: Guid option
|}

let init (props: BuildingsPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        SelectedListItems = []
        SelectedTab =
            match props.BuildingId with
            | Some buildingId -> Tab.Details buildingId
            | None -> Tab.List
        ListItems = []
        LoadingListItems = true
        CurrentBuildingId = props.CurrentBuildingId
    }
    
    let cmd =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetBuildings)
            None
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

        let updatedState = { state with SelectedListItems = newlySelectedItems }
        match state.SelectedTab with
        | Details detailId when detailId = listItem.BuildingId ->
            updatedState, Cmd.none
        | _ ->
            { updatedState with SelectedTab = Details listItem.BuildingId }
            , Routing.navigateToPage (Routing.Page.BuildingDetails listItem.BuildingId)
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.BuildingId <> listItem.BuildingId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Routing.navigateToPage Routing.Page.BuildingList
    | SelectTab tab ->
        let cmd =
            match tab with
            | List -> Routing.navigateToPage (Routing.Page.BuildingList)
            | Details buildingId -> Routing.navigateToPage (Routing.Page.BuildingDetails buildingId)
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
        state, 
            showConfirmationModal
                {|
                    Title = "Gebouw verwijderen?"
                    Message = sprintf "Bent u er zeker van dat u %s wilt verwijderen?" building.Name
                    OnConfirmed = fun () -> ConfirmRemoveListItem building
                    OnDismissed = fun () -> NoOp
                |}
    | ConfirmRemoveListItem building ->
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
        | Ok _ -> state, showSuccessToastCmd "Het gebouw is verwijderd"
        | Error DeleteBuildingError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om een gebouw te verwijderen"
        | Error DeleteBuildingError.NotFound ->
            //Do nothing... something might have gone wrong, maybe?
            printf "The building that's being deleted was not found O_o?"
            state, Cmd.none
    | RemotingError e ->
        { state with ListItems = []; LoadingListItems = false }, showGenericErrorModalCmd e
    | Created building ->
        let listItem = building.ToListItem()
        let newListItems = listItem :: state.ListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        { state with 
            ListItems = newListItems |> List.sortBy (fun b -> b.Code)
            SelectedListItems = newSelectedListItems
        }, Cmd.batch [
            SelectTab (Details listItem.BuildingId) |> Cmd.ofMsg
            showSuccessToastCmd "Het gebouw is aangemaakt"
        ]
    | Edited building ->
        let listItem = building.ToListItem()
        let newListItems = state.ListItems |> List.map (fun li -> if li.BuildingId = building.BuildingId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.BuildingId = building.BuildingId then listItem else li)
        { state with 
            ListItems = newListItems
            SelectedListItems = newSelectedListItems
        }, Cmd.batch [
            SelectTab (Details listItem.BuildingId) |> Cmd.ofMsg
            showSuccessToastCmd "Het gebouw is gewijzigd"
        ]
    | CurrentBuildingChanged building ->
        { state with CurrentBuildingId = Some building.BuildingId }, Cmd.none
    | NoOp ->
        state, Cmd.none


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
        let list (state: State) =
            [
                let currentBuildingId = state.CurrentBuildingId |> Option.defaultValue (Guid.Empty)
                SortableTable.render
                    {|
                        ListItems = state.ListItems
                        DisplayAttributes = SortableAttribute.All
                        IsSelected = Some (fun li -> li.BuildingId = currentBuildingId)
                        OnSelect = Some (CurrentBuildingChanged >> dispatch)
                        IsEditable = None
                        OnEdit = Some (AddDetailTab >> dispatch)
                        IsDeletable = None
                        OnDelete = Some (RemoveListItem >> dispatch)
                        Key = "BuildingsPageTable"
                    |}
                if state.LoadingListItems then
                    div [ Class Bootstrap.textCenter ] [
                        str "Gebouwen worden geladen..."
                    ]
                elif state.ListItems |> List.length = 0 then
                    div [ Class Bootstrap.textCenter ] [
                        str "Er werden geen resultaten gevonden..."
                    ]
            ]
            |> fragment []

        div [ Class Bootstrap.colMd12 ] [
            div [ classes [ Bootstrap.nav; Bootstrap.navTabs ] ] [
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle List); OnClick (fun _ -> SelectTab List |> dispatch) ] 
                        [ str "Overzicht" ] 
                ]
                for selected in state.SelectedListItems do
                    yield li [ Class Bootstrap.navItem ] [
                        a 
                            [ Class (determineNavItemStyle (Details selected.BuildingId)); OnClick (fun _ -> SelectTab (Details selected.BuildingId) |> dispatch) ] 
                            [ str (sprintf "%s: %s" selected.Code selected.Name) ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuw gebouw" ]
                ]
            ]

            div [ Class Bootstrap.tabContent ] [
                match state.SelectedTab with
                | List -> list state
                | Details buildingId -> 
                    BuildingDetails.render 
                        {| 
                            CurrentUser = state.CurrentUser 
                            Identifier = buildingId
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