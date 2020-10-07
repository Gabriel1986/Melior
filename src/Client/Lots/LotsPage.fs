module Client.Lots.LotsPage

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Read
open Shared.Remoting

open Client
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.SortableTable
open Client.Library

type State = {
    CurrentUser: User
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
    | ListItemRemoved of Result<LotListItem, DeleteLotError>
    | Created of Lot
    | Edited of Lot

type LotsPageProps = {|
    CurrentUser: User
    CurrentBuilding: BuildingListItem
    LotId: Guid option
|}

type SortableLotListItemAttribute =
    | OwnerName
    | Code
    | LotType
    | Floor
    | Description
    member me.ToString' () =
        match me with
        | OwnerName -> "Eigenaar"
        | Code -> "Code"
        | LotType -> "Type"
        | Floor -> "Verd."
        | Description -> "Omschrijving"
    member me.StringValueOf': LotListItem -> string =
        match me with
        | OwnerName -> (fun li ->
            match li.LegalRepresentative with 
            | Some (LotOwnerListItem.Owner o) -> o.Name 
            | Some (LotOwnerListItem.Organization o) -> o.Name
            | None -> "")
        | Code -> (fun li -> li.Code)
        | LotType -> (fun li -> string li.LotType)
        | Floor -> (fun li -> string li.Floor)
        | Description -> (fun li -> li.Description |> Option.defaultValue "")
    member me.Compare': LotListItem -> LotListItem -> int =
        match me with
        | Floor -> 
            fun li otherLi -> (defaultArg li.Floor -1000) - (defaultArg otherLi.Floor -1000)
        | _     -> 
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ LotType; Floor; Description; OwnerName; Code ]
    interface ISortableAttribute<LotListItem> with
        member me.ToString = me.ToString'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true

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
            props.CurrentBuilding.BuildingId
            (fun lots -> Loaded (lots, props.LotId))
            RemotingError
    state, cmd

let private mapCurrentOwner =
    function
    | LotOwner.Owner owner -> 
        LotOwnerListItem.Owner {| PersonId = owner.PersonId; Name = owner.FullName () |}
    | LotOwner.Organization organization -> 
        LotOwnerListItem.Organization {| OrganizationId = organization.OrganizationId; Name = organization.Name |}

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let toListItem (lot: Lot): LotListItem = {
        LotId = lot.LotId
        BuildingId = lot.BuildingId
        Code = lot.Code
        LegalRepresentative = lot.LegalRepresentative () |> Option.map mapCurrentOwner
        LotType = lot.LotType
        Floor = lot.Floor
        Description = lot.Description
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
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Routing.navigateToPage (Routing.Page.LotList { BuildingId = state.CurrentBuilding.BuildingId })
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
                (lot.BuildingId, lot.LotId)
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
        | Error DeleteLotError.AuthorizationError ->
            state, showErrorToastCmd "U heeft niet genoeg rechten om een kavel te verwijderen"
        | Error DeleteLotError.NotFound ->
            printf "The lot that was being deleted was not found in the DB... Somehow..."
            state, Cmd.none
    | RemotingError e ->
        state, showGenericErrorModalCmd e
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
        let list (state: State) =
            SortableTable.render 
                {|
                    ListItems = state.ListItems
                    DisplayAttributes = SortableLotListItemAttribute.All
                    IsSelected = None
                    OnSelect = None
                    IsEditable = None
                    OnEdit = Some (AddDetailTab >> dispatch)
                    IsDeletable = None
                    OnDelete = Some (RemoveListItem >> dispatch)
                    Key = "LotsPageTable"
                |}

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
                            [ Class (determineNavItemStyle (Details selected)); OnClick (fun _ -> SelectTab (Details selected) |> dispatch) ] 
                            [ str selected.Code ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe kavel" ]
                ]
            ]

            match state.SelectedTab with
            | List -> list state
            | Details listItem -> 
                LotDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        CurrentBuilding = state.CurrentBuilding
                        Identifier = listItem.LotId
                        IsNew = false
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
            | New ->
                LotDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        CurrentBuilding = state.CurrentBuilding
                        Identifier = Guid.NewGuid()
                        IsNew = true
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
        ]
    ]

let render (props: LotsPageProps) =
    React.elmishComponent ("LotsPage", init props, update, view)