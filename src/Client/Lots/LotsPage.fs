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
open Client.Routing

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
    | Details of lotId: Guid
    | New
type Msg =
    | AddDetailTab of LotListItem
    | RemoveDetailTab of LotListItem
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: LotListItem list * selectedListItemId: Guid option
    | RemoveListItem of LotListItem
    | ConfirmRemoveListItem of LotListItem
    | ListItemRemoved of Result<LotListItem, DeleteLotError>
    | Created of Lot
    | Edited of Lot
    | NoOp

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
    | Share
    override me.ToString () =
        match me with
        | OwnerName -> "Eigenaar"
        | Code -> "Code"
        | LotType -> "Type"
        | Floor -> "Verd."
        | Share -> "Quot."
    member me.ReactElementFor': LotListItem -> ReactElement =
        match me with
        | OwnerName -> (fun li ->
            match li.LegalRepresentative with
            | None -> 
                str ""
            | Some legalRepresentative ->
                let page =
                    match legalRepresentative with
                    | Owner owner -> 
                        Page.OwnerDetails { BuildingId = li.BuildingId; DetailId = owner.PersonId }
                    | Organization org -> 
                        Page.OrganizationDetails { BuildingId = li.BuildingId; DetailId = org.OrganizationId }

                str (me.StringValueOf' li)
                |> wrapInLink page)
        | x -> (fun li -> str (x.StringValueOf' li))
    member me.ExtraHeaderAttributes': IHTMLProp seq =
        match me with
        | Code -> seq { Style [ Width "200px" ] }
        | Share -> seq { Style [ Width "100px" ]; Title "Quotiteit / aandeel" }
        | Floor -> seq { Style [ Width "100px" ] }
        | _ -> Seq.empty
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
        | Share -> (fun li -> li.Share |> Option.map string |> Option.defaultValue "")
    member me.Compare': LotListItem -> LotListItem -> int =
        match me with
        | Floor -> 
            fun li otherLi -> (defaultArg li.Floor -1000) - (defaultArg otherLi.Floor -1000)
        | Share ->
            fun li otherLi -> (defaultArg li.Share -1000) - (defaultArg otherLi.Share -1000)
        | _     -> 
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ LotType; Code; Share; OwnerName; Floor ]
    interface ISortableAttribute<LotListItem> with
        member me.ReactElementFor = me.ReactElementFor'
        member me.ExtraHeaderAttributes = me.ExtraHeaderAttributes'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true

let init (props: LotsPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        CurrentBuilding = props.CurrentBuilding
        SelectedListItems = []
        SelectedTab =
            match props.LotId with
            | Some lotId -> Details lotId
            | None -> List
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

let private mapCurrentOwner (owner: LotOwner): LotOwnerListItem =
    match owner.LotOwnerType with
    | LotOwnerType.Owner owner -> 
        LotOwnerListItem.Owner {| PersonId = owner.PersonId; Name = owner.FullName () |}
    | LotOwnerType.Organization organization -> 
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
        Share = lot.Share
    }

    match msg with
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.LotId = listItem.LotId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.Description)
            |> List.sortBy (fun li -> li.Floor)

        let updatedState = { state with SelectedListItems = newlySelectedItems }
        match state.SelectedTab with
        | Details detailId when detailId = listItem.LotId ->
            updatedState, Cmd.none
        | _ ->
            { updatedState with SelectedTab = Details listItem.LotId }
            , Routing.navigateToPage (Routing.Page.LotDetails { BuildingId = state.CurrentBuilding.BuildingId; DetailId = listItem.LotId })
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.LotId <> listItem.LotId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Routing.navigateToPage (Routing.Page.LotList { BuildingId = state.CurrentBuilding.BuildingId })
    | SelectTab tab ->
        let buildingId = state.CurrentBuilding.BuildingId
        let cmd =
            match tab with
            | List -> Routing.navigateToPage (Routing.Page.LotList { BuildingId = buildingId })
            | Details lotId -> Routing.navigateToPage (Routing.Page.LotDetails { BuildingId = buildingId; DetailId = lotId })
            | New -> Routing.navigateToPage (Routing.Page.LotList { BuildingId = buildingId })
        { state with SelectedTab = tab }, cmd
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
        state, 
            showConfirmationModal
                {|
                    Title = "Kavel verwijderen?"
                    Message = sprintf "Bent u er zeker van dat u %s wilt verwijderen?" lot.Code
                    OnConfirmed = fun () -> ConfirmRemoveListItem lot
                    OnDismissed = fun () -> NoOp
                |}
    | ConfirmRemoveListItem lot ->
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
        | Ok _ -> state, showSuccessToastCmd "Het kavel is verwijderd"
        | Error DeleteLotError.AuthorizationError ->
            state, showErrorToastCmd "U heeft niet genoeg rechten om een kavel te verwijderen"
        | Error DeleteLotError.NotFound ->
            printf "The lot that was being deleted was not found in the DB... Somehow..."
            state, Cmd.none
    | RemotingError e ->
        { state with ListItems = []; LoadingListItems = false }, showGenericErrorModalCmd e
    | Created lot ->
        let listItem = toListItem lot
        let newListItems = listItem :: state.ListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        { state with 
            ListItems = newListItems
            SelectedListItems = newSelectedListItems
        }
        , Cmd.batch [
            SelectTab (Details listItem.LotId) |> Cmd.ofMsg
            showSuccessToastCmd "Het kavel is aangemaakt"
        ]
    | Edited lot ->
        let listItem = toListItem lot
        let newListItems = state.ListItems |> List.map (fun li -> if li.LotId = lot.LotId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.LotId = lot.LotId then listItem else li)
        { state with 
            ListItems = newListItems
            SelectedListItems = newSelectedListItems
        }
        , Cmd.batch [
            SelectTab (Details listItem.LotId) |> Cmd.ofMsg
            showSuccessToastCmd "Het kavel is gewijzigd"
        ]
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
                if state.LoadingListItems then
                    div [ Class Bootstrap.textCenter ] [
                        str "Kavels worden geladen..."
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
                            [ Class (determineNavItemStyle (Details selected.LotId)); OnClick (fun _ -> SelectTab (Details selected.LotId) |> dispatch) ] 
                            [ str selected.Code ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe kavel" ]
                ]
            ]

            div [ Class Bootstrap.tabContent ] [
                match state.SelectedTab with
                | List -> list state
                | Details lotId -> 
                    LotDetails.render 
                        {| 
                            CurrentUser = state.CurrentUser 
                            CurrentBuilding = state.CurrentBuilding
                            Identifier = lotId
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
    ]
    |> withPageHeader "Kavels"

let render (props: LotsPageProps) =
    React.elmishComponent ("LotsPage", init props, update, view)