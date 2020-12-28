module Client.Financial.Invoicing.InvoicesPage

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents
open Thoth.Elmish

open Shared.Read
open Shared.Write
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
    SelectedListItems: InvoiceListItem list
    SelectedTab: Tab
    ListItemsState: ListItemsState
    Filter: FinancialTransactionFilter
}
and Tab =
    | List
    | Details of invoiceId: Guid
    | New
and ListItemsState =
    | PreparingToLoad
    | Loading
    | Loaded of InvoiceListItem list
    member me.ListItems =
        match me with
        | PreparingToLoad
        | Loading -> []
        | Loaded listItems -> listItems

type Msg =
    | AddDetailTab of InvoiceListItem
    | RemoveDetailTab of InvoiceListItem
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: InvoiceListItem list * selectedListItem: Guid option
    | RemoveListItem of InvoiceListItem
    | ConfirmRemoveListItem of InvoiceListItem
    | ListItemRemoved of Result<InvoiceListItem, DeleteInvoiceError>
    | Created of Invoice
    | Edited of Invoice
    | GetInvoices of FinancialTransactionFilter
    | ChangeListItemsState of ListItemsState
    | NoOp

type InvoicesPageProps = {|
    CurrentUser: User
    CurrentBuilding: BuildingListItem
    InvoiceId: Guid option
|}

type SortableInvoiceListItemAttribute =
    | InvoiceNumber
    | Organization
    | DistributionKey
    | Cost
    | DueDate
    | IsPaid
    override me.ToString () =
        match me with
        | InvoiceNumber -> "Nr."
        | Organization -> "Leverancier"
        | DistributionKey -> "Sleutel"
        | Cost -> "Totaal"
        | DueDate -> "Betaaldag"
        | IsPaid -> "Betaald"
    member me.ReactElementFor': InvoiceListItem -> ReactElement =
        match me with
        | Organization -> (fun li -> 
            span [] [
                str (me.StringValueOf' li)
                |> wrapInLink (Page.OrganizationDetails { BuildingId = li.BuildingId; DetailId = li.OrganizationId })
            ])
        | x -> (fun li -> str (x.StringValueOf' li))
    member me.StringValueOf': InvoiceListItem -> string =
        match me with
        | InvoiceNumber -> (fun li -> li.LocalInvoiceNumber)
        | Organization -> (fun li -> li.OrganizationName)
        | DistributionKey -> (fun li -> li.DistributionKeyName)
        | Cost -> (fun li -> string li.Cost)
        | DueDate -> (fun li -> sprintf "%02i-%02i-%i" li.DueDate.Day li.DueDate.Month li.DueDate.Year)
        | IsPaid -> (fun li -> if li.IsPaid then "Ja" else "Nee")
    member me.Compare': InvoiceListItem -> InvoiceListItem -> int =
        match me with
        | DueDate -> 
            fun li otherLi -> li.DueDate.CompareTo(otherLi.DueDate)
        | Cost ->
            fun li otherLi -> int (li.Cost - otherLi.Cost)
        | IsPaid ->
            fun li otherLi ->
                if li.IsPaid = otherLi.IsPaid then 0 
                elif li.IsPaid && not otherLi.IsPaid then 1 else -1
        | _ ->
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ InvoiceNumber; Organization; DistributionKey; Cost; DueDate; IsPaid ]
    interface ISortableAttribute<InvoiceListItem> with
        member me.ReactElementFor = me.ReactElementFor'
        member _.ExtraHeaderAttributes = Seq.empty
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true


let getInvoices (filter: FinancialTransactionFilter, selectedTab: Tab) =
    let invoiceId =
        match selectedTab with
        | List -> None
        | Details invoiceId -> Some invoiceId
        | New -> None
    Cmd.OfAsync.either
        (Remoting.getRemotingApi()).GetInvoices filter
        (fun invoices -> Loaded (invoices, invoiceId))
        RemotingError

let init (props: InvoicesPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        CurrentBuilding = props.CurrentBuilding
        SelectedListItems = []
        SelectedTab =
            match props.InvoiceId with
            | Some invoiceId -> Tab.Details invoiceId
            | None -> Tab.List
        ListItemsState = Loading
        Filter = { 
            BuildingId = props.CurrentBuilding.BuildingId
            Period = Year (DateTime.Now.Year) 
        }
    }
    state, Cmd.none

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let toListItem (invoice: Invoice): InvoiceListItem = {
        InvoiceId = invoice.InvoiceId
        BuildingId = invoice.BuildingId
        FinancialYearCode = invoice.FinancialYear.Code
        FinancialYearIsClosed = invoice.FinancialYear.IsClosed
        InvoiceNumber = invoice.InvoiceNumber
        InvoiceDate = invoice.InvoiceDate
        Cost = invoice.Cost
        DistributionKeyName = invoice.DistributionKey.Name
        OrganizationName = invoice.Organization.Name
        OrganizationId = invoice.Organization.OrganizationId
        CategoryCode = invoice.FinancialCategory.Code
        CategoryDescription = invoice.FinancialCategory.Description
        DueDate = invoice.DueDate
        IsPaid =
            let cost = invoice.Cost
            let paidAmount = invoice.Payments |> List.sumBy (fun payment -> payment.Amount)
            cost - paidAmount = Decimal.Zero
    }

    match msg with
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.InvoiceId = listItem.InvoiceId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.LocalInvoiceNumber)
        let updatedState = { state with SelectedListItems = newlySelectedItems }
        match state.SelectedTab with
        | Details detailId when detailId = listItem.InvoiceId ->
            updatedState, Cmd.none
        | _ ->
            { updatedState with SelectedTab = Details listItem.InvoiceId }
            , Routing.navigateToPage (Routing.Page.InvoiceDetails { BuildingId = state.CurrentBuilding.BuildingId; DetailId = listItem.InvoiceId })
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.InvoiceId <> listItem.InvoiceId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Routing.navigateToPage (Routing.Page.Invoices { BuildingId = state.CurrentBuilding.BuildingId })
    | SelectTab tab ->
        { state with SelectedTab = tab }, Cmd.none
    | Loaded (invoices, selectedInvoiceId) ->
        let newState = { state with ListItemsState = ListItemsState.Loaded invoices }
        let cmd =
            match selectedInvoiceId with
            | Some selectedInvoiceId ->
                let selectedListItem = invoices |> List.tryFind (fun listItem -> listItem.InvoiceId = selectedInvoiceId)
                match selectedListItem with
                | Some selected -> AddDetailTab selected |> Cmd.ofMsg
                | None -> Cmd.none
            | None -> Cmd.none
        newState, cmd
    | ChangeListItemsState newState ->
        { state with ListItemsState = newState }, Cmd.none
    | RemoveListItem invoice ->
        state, 
            showConfirmationModal
                {|
                    Title = "Factuur verwijderen?"
                    Message = sprintf "Bent u er zeker van dat u %s wilt verwijderen?" invoice.LocalInvoiceNumber
                    OnConfirmed = fun () -> ConfirmRemoveListItem invoice
                    OnDismissed = fun () -> NoOp
                |}
    | ConfirmRemoveListItem invoice ->
        let cmd =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi().DeleteInvoice)
                (invoice.BuildingId, invoice.InvoiceId)
                (fun r -> r |> Result.map (fun _ -> invoice) |> ListItemRemoved)
                RemotingError

        let currentListItems =
            match state.ListItemsState with
            | ListItemsState.PreparingToLoad
            | ListItemsState.Loading -> []
            | ListItemsState.Loaded listItems -> listItems

        let newSelection =
            state.SelectedListItems |> List.filter (fun selected -> selected.InvoiceId <> invoice.InvoiceId)

        let newItems =
            currentListItems |> List.filter (fun item -> item.InvoiceId <> invoice.InvoiceId)

        { state with SelectedListItems = newSelection; ListItemsState = ListItemsState.Loaded newItems }, cmd
    | ListItemRemoved result ->
        match result with
        | Ok _ -> state, Cmd.none
        | Error DeleteInvoiceError.AuthorizationError ->
            state, showErrorToastCmd "U heeft niet genoeg rechten om een factuur te verwijderen"
        | Error DeleteInvoiceError.NotFound ->
            printf "The invoice that was being deleted was not found in the DB... Somehow..."
            state, showErrorToastCmd "De factuur werd niet gevonden in de databank"
    | RemotingError e ->
        { state with ListItemsState = ListItemsState.Loaded [] }, showGenericErrorModalCmd e
    | Created invoice ->
        let currentListItems = state.ListItemsState.ListItems
        let listItem = toListItem invoice
        let newListItems = listItem :: currentListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        { state with ListItemsState = ListItemsState.Loaded newListItems; SelectedListItems = newSelectedListItems }
        , showSuccessToastCmd "De factuur is aangemaakt"
    | Edited invoice ->
        let currentListItems = state.ListItemsState.ListItems
        let listItem = toListItem invoice
        let newListItems = currentListItems |> List.map (fun li -> if li.InvoiceId = invoice.InvoiceId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.InvoiceId = invoice.InvoiceId then listItem else li)
        { state with ListItemsState = ListItemsState.Loaded newListItems; SelectedListItems = newSelectedListItems }
        , showSuccessToastCmd "De factuur is gewijzigd"
    | GetInvoices filter ->
        { state with ListItemsState = ListItemsState.Loading }, getInvoices (filter, state.SelectedTab)
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
                            FilterKey = "Invoicefilter"
                            OnPrepareToChangeFilter = fun _ -> ChangeListItemsState ListItemsState.PreparingToLoad |> dispatch
                            OnFilterChanged = GetInvoices >> dispatch
                            OnError = RemotingError >> dispatch
                            OnlyShowFinancialYears = false
                        |}
                ]
                SortableTable.render 
                    {|
                        ListItems = state.ListItemsState.ListItems
                        DisplayAttributes = SortableInvoiceListItemAttribute.All
                        IsSelected = None
                        OnSelect = None
                        IsEditable = Some (fun li -> not li.FinancialYearIsClosed)
                        OnEdit = Some (AddDetailTab >> dispatch)
                        IsDeletable = Some (fun li -> not li.FinancialYearIsClosed)
                        OnDelete = Some (RemoveListItem >> dispatch)
                        Key = "InvoicesPageTable"
                    |}
                match state.ListItemsState with
                | ListItemsState.PreparingToLoad
                | ListItemsState.Loading ->
                    div [ Class Bootstrap.textCenter ] [
                        str "Facturen worden geladen..."
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
                            [ Class (determineNavItemStyle (Details selected.InvoiceId)); OnClick (fun _ -> SelectTab (Details selected.InvoiceId) |> dispatch) ] 
                            [ str selected.LocalInvoiceNumber ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe factuur" ]
                ]
            ]

            div [ Class Bootstrap.tabContent ] [
                match state.SelectedTab with
                | List -> list state
                | Details invoiceId -> 
                    InvoiceDetails.render 
                        {| 
                            CurrentUser = state.CurrentUser 
                            CurrentBuilding = state.CurrentBuilding
                            Identifier = invoiceId
                            IsNew = false
                            NotifyCreated = Created >> dispatch
                            NotifyEdited = Edited >> dispatch
                            NotifyPaymentAdded = fun _ -> GetInvoices (state.Filter) |> dispatch
                            NotifyPaymentUpdated = fun _ -> GetInvoices (state.Filter) |> dispatch
                        |}
                | New ->
                    InvoiceDetails.render 
                        {| 
                            CurrentUser = state.CurrentUser 
                            CurrentBuilding = state.CurrentBuilding
                            Identifier = Guid.NewGuid()
                            IsNew = true
                            NotifyCreated = Created >> dispatch
                            NotifyEdited = Edited >> dispatch
                            NotifyPaymentAdded = fun _ -> GetInvoices (state.Filter) |> dispatch
                            NotifyPaymentUpdated = fun _ -> GetInvoices (state.Filter) |> dispatch
                        |}
            ]
        ]
    ]
    |> withPageHeader "Facturen"

let render (props: InvoicesPageProps) =
    React.elmishComponent ("InvoicesPage", init props, update, view)