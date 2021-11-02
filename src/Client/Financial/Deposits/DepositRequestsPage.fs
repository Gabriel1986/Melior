module Client.Financial.Deposits.DepositRequestsPage

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Read
open Shared.Remoting
open Shared.Library

open Client
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.SortableTable
open Client.Library
open Client.Routing

type State = {
    CurrentUser: User
    CurrentBuilding: BuildingListItem
    SelectedListItems: DepositRequestListItem list
    SelectedTab: Tab
    ListItemsState: ListItemsState
    Filter: FinancialTransactionFilter
}
and Tab =
    | List
    | Details of depositRequestId: Guid
    | New
and ListItemsState =
    | PreparingToLoad
    | Loading
    | Loaded of DepositRequestListItem list
    member me.ListItems =
        match me with
        | PreparingToLoad
        | Loading -> []
        | Loaded listItems -> listItems

type Msg =
    | AddDetailTab of DepositRequestListItem
    | RemoveDetailTab of DepositRequestListItem
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: DepositRequestListItem list * selectedListItem: Guid option
    | RemoveListItem of DepositRequestListItem
    | ConfirmRemoveListItem of DepositRequestListItem
    | ListItemRemoved of Result<DepositRequestListItem, DeleteDepositRequestError>
    | Created of DepositRequest
    | Edited of DepositRequest
    | GetRequests of FinancialTransactionFilter
    | ChangeListItemsState of ListItemsState
    | NoOp

type DepositRequestsPageProps = {|
    CurrentUser: User
    CurrentBuilding: BuildingListItem
    DepositRequestId: Guid option
|}

type SortableOwnerDepositRequestListItemAttribute =
    | RequestNumber
    | DistributionKey
    | Amount
    | DueDate
    | IsPaid
    override me.ToString () =
        match me with
        | RequestNumber -> "Nr."
        | DistributionKey -> "Sleutel"
        | Amount -> "Totaal"
        | DueDate -> "Betaaldag"
        | IsPaid -> "Betaald"
    member me.ReactElementFor': DepositRequestListItem -> ReactElement =
        fun li -> str (me.StringValueOf' li)
    member me.StringValueOf': DepositRequestListItem -> string =
        match me with
        | RequestNumber -> (fun li -> li.LocalRequestNumber)
        | DistributionKey -> (fun li -> li.DistributionKeyName)
        | Amount -> (fun li -> String.Format("€{0:0.00}" , li.Amount).Replace(".", ","))
        | DueDate -> (fun li -> sprintf "%02i-%02i-%i" li.DueDate.Day li.DueDate.Month li.DueDate.Year)
        | IsPaid -> (fun li -> if li.IsPaid then "Ja" else "Nee")
    member me.Compare': DepositRequestListItem -> DepositRequestListItem -> int =
        match me with
        | DueDate -> 
            fun li otherLi -> li.DueDate.CompareTo(otherLi.DueDate)
        | Amount ->
            fun li otherLi -> int (li.Amount - otherLi.Amount)
        | IsPaid ->
            fun li otherLi ->
                if li.IsPaid = otherLi.IsPaid then 0 
                elif li.IsPaid && not otherLi.IsPaid then 1 else -1
        | _ ->
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ RequestNumber; DistributionKey; Amount; DueDate; IsPaid ]
    interface ISortableAttribute<DepositRequestListItem> with
        member me.ReactElementFor = me.ReactElementFor'
        member _.ExtraHeaderAttributes = Seq.empty
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true

let getDepositRequests (filter: FinancialTransactionFilter, selectedTab: Tab) =
    let requestId =
        match selectedTab with
        | List -> None
        | Details requestId -> Some requestId
        | New -> None
    Cmd.OfAsync.either
        (Remoting.getRemotingApi()).GetDepositRequests filter
        (fun requests -> Loaded (requests, requestId))
        RemotingError

let init (props: DepositRequestsPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        CurrentBuilding = props.CurrentBuilding
        SelectedListItems = []
        SelectedTab =
            match props.DepositRequestId with
            | Some requestId -> Tab.Details requestId
            | None -> Tab.List
        ListItemsState = Loading
        Filter = { 
            BuildingId = props.CurrentBuilding.BuildingId
            Period = Year (DateTime.Now.Year) 
        }
    }
    state, Cmd.none

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let toListItem (request: DepositRequest): DepositRequestListItem = {
        DepositRequestId = request.DepositRequestId
        DepositRequestNumber = request.DepositRequestNumber
        BuildingId = request.BuildingId
        FinancialYearCode = request.FinancialYear.Code
        FinancialYearIsClosed = request.FinancialYear.IsClosed
        RequestDate = request.RequestDate
        BookingDate = request.BookingDate
        DueDate = request.DueDate
        Amount = request.Amount
        DistributionKeyName = request.DistributionKey.Name
        ToFinancialCategoryCode = request.ToFinancialCategory.Code
        ToFinancialCategoryDescription = request.ToFinancialCategory.Description
        IsPaid =
            let cost = request.Amount
            let paidAmount = request.Deposits |> List.sumBy (fun payment -> payment.Amount)
            cost - paidAmount = Decimal.Zero
    }

    match msg with
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.DepositRequestId = listItem.DepositRequestId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.LocalRequestNumber)
        let updatedState = { state with SelectedListItems = newlySelectedItems }
        match state.SelectedTab with
        | Details detailId when detailId = listItem.DepositRequestId ->
            updatedState, Cmd.none
        | _ ->
            { updatedState with SelectedTab = Details listItem.DepositRequestId }
            , Routing.navigateToPage (Routing.Page.DepositRequestDetails { BuildingId = state.CurrentBuilding.BuildingId; DetailId = listItem.DepositRequestId })
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.DepositRequestId <> listItem.DepositRequestId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Routing.navigateToPage (Routing.Page.DepositRequests { BuildingId = state.CurrentBuilding.BuildingId })
    | SelectTab tab ->
        { state with SelectedTab = tab }, Cmd.none
    | Loaded (requests, selectedDepositRequestId) ->
        let newState = { state with ListItemsState = ListItemsState.Loaded requests }
        let cmd =
            match selectedDepositRequestId with
            | Some selectedDepositRequestId ->
                let selectedListItem = requests |> List.tryFind (fun listItem -> listItem.DepositRequestId = selectedDepositRequestId)
                match selectedListItem with
                | Some selected -> AddDetailTab selected |> Cmd.ofMsg
                | None -> Cmd.none
            | None -> Cmd.none
        newState, cmd
    | RemoveListItem request ->
        state, 
            showConfirmationModal
                {|
                    Title = "Voorschotaanvraag verwijderen?"
                    Message = sprintf "Bent u er zeker van dat u %s wilt verwijderen?" request.LocalRequestNumber
                    OnConfirmed = fun () -> ConfirmRemoveListItem request
                    OnDismissed = fun () -> NoOp
                |}
    | ConfirmRemoveListItem request ->
        let cmd =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi().DeleteDepositRequest)
                (request.BuildingId, request.DepositRequestId)
                (fun r -> r |> Result.map (fun _ -> request) |> ListItemRemoved)
                RemotingError

        let currentListItems =
            match state.ListItemsState with
            | ListItemsState.PreparingToLoad
            | ListItemsState.Loading -> []
            | ListItemsState.Loaded listItems -> listItems

        let newSelection =
            state.SelectedListItems |> List.filter (fun selected -> selected.DepositRequestId <> request.DepositRequestId)

        let newItems =
            currentListItems |> List.filter (fun item -> item.DepositRequestId <> request.DepositRequestId)

        { state with SelectedListItems = newSelection; ListItemsState = ListItemsState.Loaded newItems }, cmd
    | ListItemRemoved result ->
        match result with
        | Ok _ -> state, Cmd.none
        | Error DeleteDepositRequestError.AuthorizationError ->
            state, showErrorToastCmd "U heeft niet genoeg rechten om een aanvraag te verwijderen"
        | Error DeleteDepositRequestError.NotFound ->
            printf "The request that was being deleted was not found in the DB... Somehow..."
            state, showErrorToastCmd "De aanvraag werd niet gevonden in de databank"
    | RemotingError e ->
        { state with ListItemsState = ListItemsState.Loaded [] }, showGenericErrorModalCmd e
    | Created request ->
        let currentListItems = state.ListItemsState.ListItems
        let listItem = toListItem request
        let newListItems = listItem :: currentListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        { state with ListItemsState = ListItemsState.Loaded newListItems; SelectedListItems = newSelectedListItems }
        , showSuccessToastCmd "De aanvraag is aangemaakt"
    | Edited request ->
        let currentListItems = state.ListItemsState.ListItems
        let listItem = toListItem request
        let newListItems = currentListItems |> List.map (fun li -> if li.DepositRequestId = request.DepositRequestId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.DepositRequestId = request.DepositRequestId then listItem else li)
        { state with ListItemsState = ListItemsState.Loaded newListItems; SelectedListItems = newSelectedListItems }
        , showSuccessToastCmd "De aanvraag is gewijzigd"
    | GetRequests filter ->
        { state with ListItemsState = ListItemsState.Loading }, getDepositRequests (filter, state.SelectedTab)
    | ChangeListItemsState newListItemsState ->
        { state with ListItemsState = newListItemsState }, Cmd.none
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
                let disableFilter =
                    match state.ListItemsState with
                    | Loading -> true
                    | _ -> false
                fieldset [ Disabled disableFilter ] [
                    Client.Financial.FinancialTransactionFilterComponent.render
                        {|
                            CurrentBuildingId = state.CurrentBuilding.BuildingId
                            FilterKey = "DepositRequestfilter"
                            OnPrepareToChangeFilter = fun _ -> ChangeListItemsState ListItemsState.PreparingToLoad |> dispatch
                            OnFilterChanged = GetRequests >> dispatch
                            OnError = RemotingError >> dispatch
                            OnlyShowFinancialYears = false
                        |}
                ]
                SortableTable.render 
                    {|
                        ListItems = state.ListItemsState.ListItems
                        DisplayAttributes = SortableOwnerDepositRequestListItemAttribute.All
                        IsSelected = None
                        OnSelect = None
                        IsEditable = Some (fun li -> not li.FinancialYearIsClosed)
                        OnEdit = Some (AddDetailTab >> dispatch)
                        IsDeletable = Some (fun li -> not li.FinancialYearIsClosed)
                        OnDelete = Some (RemoveListItem >> dispatch)
                        Key = "DepositRequestsPageTable"
                    |}
                match state.ListItemsState with
                | ListItemsState.PreparingToLoad
                | ListItemsState.Loading ->
                    div [ Class Bootstrap.textCenter ] [
                        str "Aanvragen worden geladen..."
                    ]
                | ListItemsState.Loaded [] ->
                    div [ Class Bootstrap.textCenter ] [
                        str "Er werden geen resultaten gevonden..."
                    ]
                | ListItemsState.Loaded _ ->
                    null
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
                            [ Class (determineNavItemStyle (Details selected.DepositRequestId)); OnClick (fun _ -> SelectTab (Details selected.DepositRequestId) |> dispatch) ] 
                            [ str selected.LocalRequestNumber ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe aanvraag voor voorschot" ]
                ]
            ]

            div [ Class Bootstrap.tabContent ] [
                match state.SelectedTab with
                | List -> list state
                | Details requestId -> 
                    DepositRequestDetails.render 
                        {| 
                            CurrentUser = state.CurrentUser 
                            CurrentBuilding = state.CurrentBuilding
                            Identifier = requestId
                            IsNew = false
                            NotifyCreated = Created >> dispatch
                            NotifyEdited = Edited >> dispatch
                        |}
                | New ->
                    DepositRequestDetails.render 
                        {| 
                            CurrentUser = state.CurrentUser 
                            CurrentBuilding = state.CurrentBuilding
                            Identifier = Guid.NewGuid()
                            IsNew = true
                            NotifyCreated = Created >> dispatch
                            NotifyEdited = Edited >> dispatch
                        |}
            ]
        ]
    ]
    |> withPageHeader "Voorschot aanvragen"

let render (props: DepositRequestsPageProps) =
    React.elmishComponent ("DepositRequestPage", init props, update, view)