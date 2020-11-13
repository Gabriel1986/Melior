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
    override me.ToString () =
        match me with
        | Name -> "Naam"
        | DistributionType -> "Verdeling"
        | NbMatchingLots -> "# Kavels"
    member me.ExtraHeaderAttributes': IHTMLProp seq =
        match me with
        | NbMatchingLots -> seq { Style [ Width "100px" ] }
        | _ -> Seq.empty
    member me.ReactElementFor': DistributionKeyModel -> ReactElement =
        fun li -> str (me.StringValueOf' li) 
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
                me.StringValueOf'(li).CompareTo(me.StringValueOf'(otherLi))
    static member All = [ Name; DistributionType; NbMatchingLots ]
    interface ISortableAttribute<DistributionKeyModel> with
        member me.ReactElementFor = me.ReactElementFor'
        member me.ExtraHeaderAttributes = me.ExtraHeaderAttributes'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true

type State = {
    CurrentUser: User
    CurrentBuildingId: Guid
    SelectedDistributionKeyId: Guid option
    LoadingListItems: bool
    DistributionKeys: DistributionKey list option
    Lots: LotListItem list option
    ListItems: DistributionKeyModel list
    SelectedListItems: DistributionKeyModel list
    SelectedTab: Tab
}
and Tab =
    | List
    | Details of distributionKeyId: Guid
    | New

type Msg =
    | DistributionKeysLoaded of DistributionKey list 
    | LotsLoaded of LotListItem list
    | AddDetailTab of DistributionKeyModel
    | RemoveDetailTab of DistributionKeyModel
    | SelectTab of Tab
    | RemotingError of exn
    | RemoveListItem of DistributionKeyModel
    | ConfirmRemoveListItem of DistributionKeyModel
    | ListItemRemoved of Result<DistributionKeyModel, DeleteDistributionKeyError>
    | Created of DistributionKeyModel
    | Edited of DistributionKeyModel
    | CurrentDistributionKeyChanged of DistributionKeyModel
    | NoOp

type DistributionKeysPageProps = {| CurrentUser: User; CurrentBuildingId: BuildingId; DistributionKeyId: Guid option |}

let init (props: DistributionKeysPageProps) =
    {
        CurrentUser = props.CurrentUser
        CurrentBuildingId = props.CurrentBuildingId
        SelectedDistributionKeyId = props.DistributionKeyId
        LoadingListItems = true
        DistributionKeys = None
        Lots = None
        ListItems = []
        SelectedListItems = []
        SelectedTab =
            match props.DistributionKeyId with
            | Some distributionKeyId -> Tab.Details distributionKeyId
            | None -> Tab.List
    },
    Cmd.batch [
        Cmd.OfAsync.either (Remoting.getRemotingApi()).GetDistributionKeys props.CurrentBuildingId DistributionKeysLoaded RemotingError
        Cmd.OfAsync.either (Remoting.getRemotingApi()).GetLots props.CurrentBuildingId LotsLoaded RemotingError
    ]

let private evolve (state: State) =
    match state.DistributionKeys, state.Lots with
    | (Some keys, Some lots) ->
        let models = keys |> List.map (DistributionKeyModel.FromBackendModel lots)
        let state = { state with ListItems = models }
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
        evolve { state with DistributionKeys = Some distributionKeys; LoadingListItems = false }
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

        let updatedState = { state with SelectedListItems = newlySelectedItems }
        match state.SelectedTab with
        | Details detailId when detailId = listItem.DistributionKeyId ->
            updatedState, Cmd.none
        | _ ->
            { updatedState with SelectedTab = Details listItem.DistributionKeyId }
            , Routing.navigateToPage (Routing.Page.DistributionKeyDetails { BuildingId = state.CurrentBuildingId; DetailId = listItem.DistributionKeyId })
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.BuildingId <> listItem.BuildingId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Routing.navigateToPage (Routing.Page.DistributionKeyList { BuildingId = state.CurrentBuildingId })
    | SelectTab tab ->
        { state with SelectedTab = tab }, Cmd.none
    | RemoveListItem distributionKey ->
        state, 
            showConfirmationModal
                {|
                    Title = "Verdeelsleutel verwijderen?"
                    Message = sprintf "Bent u er zeker van dat u %s wilt verwijderen?" distributionKey.Name
                    OnConfirmed = fun () -> ConfirmRemoveListItem distributionKey
                    OnDismissed = fun () -> NoOp
                |}
    | ConfirmRemoveListItem distributionKey ->
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
        | Ok _ -> 
            state, showSuccessToastCmd "De verdeeldsleutel werd verwijderd"
        | Error DeleteDistributionKeyError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om een verdeelsleutel te verwijderen"
        | Error DeleteDistributionKeyError.NotFound ->
            //Do nothing... something might have gone wrong, maybe?
            printf "The building that's being deleted was not found O_o?"
            state, showErrorToastCmd "De verdeelsleutel werd niet gevonden in de databank"
    | Created distributionKey ->
        let newListItems = distributionKey :: state.ListItems
        let newSelectedListItems = [ distributionKey ] |> List.append state.SelectedListItems
        { state with 
            ListItems = newListItems |> List.sortBy (fun b -> b.Name)
            SelectedListItems = newSelectedListItems
        }, Cmd.batch [
            showSuccessToastCmd "De verdeelsleutel is aangemaakt"
            (SelectTab (Tab.Details distributionKey.DistributionKeyId)) |> Cmd.ofMsg
        ]
    | Edited distributionKey ->
        let listItem = distributionKey
        let newListItems = state.ListItems |> List.map (fun li -> if li.DistributionKeyId = distributionKey.DistributionKeyId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.DistributionKeyId = distributionKey.DistributionKeyId then listItem else li)
        { state with 
            ListItems = newListItems
            SelectedListItems = newSelectedListItems
        }, Cmd.batch [
            showSuccessToastCmd "De verdeelsleutel is gewijzigd"
            (SelectTab (Tab.Details listItem.DistributionKeyId)) |> Cmd.ofMsg
        ]
    | CurrentDistributionKeyChanged distributionKey ->
        { state with SelectedDistributionKeyId = Some distributionKey.DistributionKeyId }, Cmd.none
    | NoOp ->
        state, Cmd.none

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
            [
                SortableTable.render 
                    {|
                        ListItems = state.ListItems
                        DisplayAttributes = SortableAttribute.All
                        IsSelected = None
                        OnSelect = None
                        IsEditable = Some (fun li -> li.CanBeEdited)
                        OnEdit = Some (AddDetailTab >> dispatch)
                        IsDeletable = Some (fun li -> li.CanBeEdited)
                        OnDelete = Some (RemoveListItem >> dispatch)
                        Key = "DistributionKeysPageTable"
                    |}
                if state.LoadingListItems then
                    div [ Class Bootstrap.textCenter ] [
                        str "Verdeelsleutels worden geladen..."
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
                            [ Class (determineNavItemStyle (Details selected.DistributionKeyId)); OnClick (fun _ -> SelectTab (Details selected.DistributionKeyId) |> dispatch) ] 
                            [ str selected.Name ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe verdeelsleutel" ]
                ]
            ]

            div [ Class Bootstrap.tabContent ] [
                match state.SelectedTab with
                | List -> list state
                | Details distributionKeyId -> 
                    DistributionKeyDetails.render 
                        {| 
                            CurrentUser = state.CurrentUser 
                            CurrentBuildingId = Some state.CurrentBuildingId
                            AllLots = state.Lots.Value
                            Identifier = distributionKeyId
                            IsNew = false
                            NotifyCreated = fun b -> dispatch (Created b)
                            NotifyEdited = fun b -> dispatch (Edited b)
                        |}
                | New ->
                    DistributionKeyDetails.render 
                        {| 
                            CurrentUser = state.CurrentUser 
                            CurrentBuildingId = Some state.CurrentBuildingId
                            AllLots = state.Lots.Value
                            Identifier = Guid.NewGuid()
                            IsNew = true
                            NotifyCreated = fun b -> dispatch (Created b)
                            NotifyEdited = fun b -> dispatch (Edited b)
                        |}
            ]
        ]
    ]

let render (props: DistributionKeysPageProps) =
    React.elmishComponent ("DistributionKeysPage", init props, update, view)