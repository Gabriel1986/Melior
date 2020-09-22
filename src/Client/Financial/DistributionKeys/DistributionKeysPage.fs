module Client.Financial.DistributionKeys.DistributionKeysPage

open System
open Elmish
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents
open Shared.Read
open Shared.Remoting
open Client
open Client.Library
open Client.SortableTable
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Types

type SortableAttribute =
    | Name
    | DistributionType
    | NbMatchingLots
    member me.ToString' () =
        match me with
        | Name -> "Naam"
        | DistributionType -> "Type"
        | NbMatchingLots -> "Lots"
    member me.StringValueOf': DistributionKeyModel -> string =
        match me with
        | Name -> (fun li -> li.Name)
        | DistributionType -> (fun li -> string li.DistributionType)
        | NbMatchingLots -> (fun li -> string li.MatchingLots.Length)
    member me.Compare': DistributionKeyModel -> DistributionKeyModel -> int =
        fun li otherLi ->
            match me with
            | NbMatchingLots ->
                li.MatchingLots.Length - otherLi.MatchingLots.Length
            | _ ->
                let result = me.StringValueOf'(li).CompareTo(me.StringValueOf'(otherLi))
                result
    static member All = [ Name; DistributionType; NbMatchingLots ]
    interface ISortableAttribute<DistributionKeyModel> with
        member me.ToString = me.ToString'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true

type State = {
    CurrentUser: User
    CurrentBuildingId: Guid
    SelectedDistributionKeyId: Guid option
    IsLoading: bool
    DistributionKeys: DistributionKey list option
    Lots: LotListItem list option
    ListItems: DistributionKeyModel list
    SelectedListItems: DistributionKeyModel list
    SelectedTab: Tab
}
and Tab =
    | List
    | Details of DistributionKeyModel
    | New

type Msg =
    | DistributionKeysLoaded of DistributionKey list 
    | LotsLoaded of LotListItem list
    | AddDetailTab of DistributionKeyModel
    | RemoveDetailTab of DistributionKeyModel
    | SelectTab of Tab
    | RemotingError of exn
    | RemoveListItem of DistributionKeyModel
    | ListItemRemoved of Result<DistributionKeyModel, DeleteDistributionKeyError>
    | Created of DistributionKeyModel
    | Edited of DistributionKeyModel
    | CurrentDistributionKeyChanged of DistributionKeyModel

type DistributionKeysPageProps = {| CurrentUser: User; CurrentBuildingId: BuildingId; DistributionKeyId: Guid option |}

let init (props: DistributionKeysPageProps) =
    {
        CurrentUser = props.CurrentUser
        CurrentBuildingId = props.CurrentBuildingId
        SelectedDistributionKeyId = props.DistributionKeyId
        IsLoading = true
        DistributionKeys = None
        Lots = None
        ListItems = []
        SelectedListItems = []
        SelectedTab = Tab.List
    },
    Cmd.batch [
        Cmd.OfAsync.either (Remoting.getRemotingApi()).GetDistributionKeys props.CurrentBuildingId DistributionKeysLoaded RemotingError
        Cmd.OfAsync.either (Remoting.getRemotingApi()).GetLots props.CurrentBuildingId LotsLoaded RemotingError
    ]

let private createModel (lots: LotListItem list) (key: DistributionKey): DistributionKeyModel = {
    DistributionKeyId = key.DistributionKeyId
    BuildingId = key.BuildingId
    Name = key.Name
    DistributionType = key.DistributionType
    CanBeEdited = key.BuildingId.IsSome
    MatchingLots =
        match key.LotsOrLotTypes with
        | LotsOrLotTypes.Lots lotIds -> lots |> List.filter (fun lot -> lotIds |> List.contains(lot.LotId))
        | LotsOrLotTypes.LotTypes types -> lots |> List.filter (fun lot -> types |> List.contains(lot.LotType))
}

let private evolve (state: State) =
    match state.DistributionKeys, state.Lots with
    | (Some keys, Some lots) ->
        let models = keys |> List.map (createModel lots)
        let state = { state with IsLoading = false; ListItems = models }
        let cmd = 
            let selectedOpt = 
                state.SelectedDistributionKeyId
                |> Option.bind (fun selectedId -> models |> List.tryFind (fun model -> model.DistributionKeyId = selectedId))
            match selectedOpt with
            | Some selected -> Cmd.ofMsg (AddDetailTab selected)
            | None -> Cmd.none
        state, cmd
    | _ -> 
        state, Cmd.none

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | DistributionKeysLoaded distributionKeys ->
        evolve { state with DistributionKeys = Some distributionKeys }
    | LotsLoaded lots ->
        evolve { state with Lots = Some lots }
    | RemotingError error ->
        state, showGenericErrorModalCmd error
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.BuildingId = listItem.BuildingId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.Name)
        { state with SelectedListItems = newlySelectedItems; SelectedTab = Details listItem }, Routing.navigateToPage (Routing.Page.DistributionKeyDetails { BuildingId = state.CurrentBuildingId; DetailId = listItem.DistributionKeyId })
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.BuildingId <> listItem.BuildingId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Cmd.none
    | SelectTab tab ->
        { state with SelectedTab = tab }, Cmd.none
    | RemoveListItem distributionKey ->
        let cmd =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi().DeleteDistributionKey)
                (state.CurrentBuildingId, distributionKey.DistributionKeyId)
                (fun r -> r |> Result.map (fun _ -> distributionKey) |> ListItemRemoved)
                RemotingError

        let newSelection =
            state.SelectedListItems |> List.filter (fun selected -> selected.DistributionKeyId <> distributionKey.DistributionKeyId)

        let newItems =
            state.ListItems |> List.filter (fun item -> item.DistributionKeyId <> distributionKey.DistributionKeyId)

        { state with SelectedListItems = newSelection; ListItems = newItems }, cmd
    | ListItemRemoved result ->
        match result with
        | Ok _ -> state, Cmd.none
        | Error DeleteDistributionKeyError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om een gebouw te verwijderen"
        | Error DeleteDistributionKeyError.NotFound ->
            //Do nothing... something might have gone wrong, maybe?
            printf "The building that's being deleted was not found O_o?"
            state, Cmd.none
    | Created distributionKey ->
        let newListItems = distributionKey :: state.ListItems
        let newSelectedListItems = [ distributionKey ] |> List.append state.SelectedListItems
        { state with 
            ListItems = newListItems |> List.sortBy (fun b -> b.Name)
            SelectedListItems = newSelectedListItems
        }, (SelectTab (Tab.Details distributionKey)) |> Cmd.ofMsg
    | Edited distributionKey ->
        let listItem = distributionKey
        let newListItems = state.ListItems |> List.map (fun li -> if li.DistributionKeyId = distributionKey.DistributionKeyId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.DistributionKeyId = distributionKey.DistributionKeyId then listItem else li)
        { state with 
            ListItems = newListItems
            SelectedListItems = newSelectedListItems
        }, (SelectTab (Tab.Details listItem)) |> Cmd.ofMsg
    | CurrentDistributionKeyChanged distributionKey ->
        { state with SelectedDistributionKeyId = Some distributionKey.DistributionKeyId }, Cmd.none

let view (state: State) (dispatch: Msg -> unit) =
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
                    DisplayAttributes = SortableAttribute.All
                    IsSelected = None
                    OnSelect = None
                    IsEditable = None
                    OnEdit = Some (AddDetailTab >> dispatch)
                    IsDeletable = None
                    OnDelete = Some (RemoveListItem >> dispatch)
                    Key = "DistributionKeysPageTable"
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
                            [ str selected.Name ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe verdeelsleutel" ]
                ]
            ]

            match state.SelectedTab with
            | List -> list state
            | Details listItem -> 
                DistributionKeyDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        CurrentBuildingId = Some state.CurrentBuildingId
                        AllLots = state.Lots.Value
                        DistributionKey = Some listItem
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
            | New ->
                DistributionKeyDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        CurrentBuildingId = Some state.CurrentBuildingId
                        AllLots = state.Lots.Value
                        DistributionKey = None
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
        ]
    ]

let render (props: DistributionKeysPageProps) =
    React.elmishComponent ("DistributionKeysPage", init props, update, view)