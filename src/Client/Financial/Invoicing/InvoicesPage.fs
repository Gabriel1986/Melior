module Client.Financial.Invoicing.InvoicesPage

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
    SelectedListItems: InvoiceListItem list
    SelectedTab: Tab
    LoadingListItems: bool
    ListItems: InvoiceListItem list
    LoadingFinancialYears: bool
    FinancialYears: FinancialYear list
    Filter: InvoiceFilter option
}
and Tab =
    | List
    | Details of InvoiceListItem
    | New
type Msg =
    | AddDetailTab of InvoiceListItem
    | RemoveDetailTab of InvoiceListItem
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: InvoiceListItem list * selectedListItem: Guid option
    | FinancialYearsLoaded of financialYears: FinancialYear list
    | RemoveListItem of InvoiceListItem
    | ConfirmRemoveListItem of InvoiceListItem
    | ListItemRemoved of Result<InvoiceListItem, DeleteInvoiceError>
    | Created of Invoice
    | Edited of Invoice
    | NoOp

type InvoicesPageProps = {|
    CurrentUser: User
    CurrentBuilding: BuildingListItem
    InvoiceId: Guid option
|}


//InvoiceId: Guid
//BuildingId: BuildingId
//FinancialYearCode: string
//InvoiceNumber: int
//Cost: int
//VatRate: int
//DistributionKeyName: string
//OrganizationName: string
//CategoryCode: string //Boekhoudkundige rekening
//DueDate: DateTimeOffset
//HasLinkedDocuments: bool
//HasBeenPaid: bool

type SortableInvoiceListItemAttribute =
    | InvoiceNumber
    | Organization
    | DistributionKey
    | Cost
    | DueDate
    member me.ToString' () =
        match me with
        | InvoiceNumber -> "Nr."
        | Organization -> "Leverancier"
        | DistributionKey -> "Sleutel"
        | Cost -> "Totaal"
        | DueDate -> "Betaaldag"
    member me.StringValueOf': InvoiceListItem -> string =
        match me with
        | InvoiceNumber -> (fun li -> li.LocalInvoiceNumber)
        | Organization -> (fun li -> li.OrganizationName)
        | DistributionKey -> (fun li -> li.DistributionKeyName)
        | Cost -> (fun li -> string li.Cost)
        | DueDate -> (fun li -> sprintf "%02i-%02i-%i" li.DueDate.Day li.DueDate.Month li.DueDate.Year)
    member me.Compare': InvoiceListItem -> InvoiceListItem -> int =
        match me with
        | DueDate -> 
            fun li otherLi -> li.DueDate.CompareTo(otherLi.DueDate)
        | Cost ->
            fun li otherLi -> int li.Cost - int otherLi.Cost
        | _     -> 
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ InvoiceNumber; Organization; DistributionKey; Cost; DueDate ]
    interface ISortableAttribute<InvoiceListItem> with
        member me.ToString = me.ToString'
        member me.ToLongString = me.ToString'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true

let init (props: InvoicesPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        CurrentBuilding = props.CurrentBuilding
        SelectedListItems = []
        SelectedTab = List
        ListItems = []
        LoadingListItems = true
        FinancialYears = []
        LoadingFinancialYears = true
        Filter = None
    }

    let cmd =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetFinancialYears)
            props.CurrentBuilding.BuildingId
            (fun years -> FinancialYearsLoaded years)
            RemotingError
    state, cmd

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
        CategoryCode = invoice.FinancialCategory.Code
        CategoryDescription = invoice.FinancialCategory.Description
        DueDate = invoice.DueDate
    }

    match msg with
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.InvoiceId = listItem.InvoiceId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.LocalInvoiceNumber)
        { state with SelectedListItems = newlySelectedItems; SelectedTab = Details listItem }, Routing.navigateToPage (Routing.Page.InvoiceDetails { BuildingId = state.CurrentBuilding.BuildingId; DetailId = listItem.InvoiceId })
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.InvoiceId <> listItem.InvoiceId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Routing.navigateToPage (Routing.Page.Invoices { BuildingId = state.CurrentBuilding.BuildingId })
    | SelectTab tab ->
        { state with SelectedTab = tab }, Cmd.none
    | Loaded (invoices, selectedInvoiceId) ->
        let newState = { state with ListItems = invoices; LoadingListItems = false }
        let cmd =
            match selectedInvoiceId with
            | Some selectedInvoiceId ->
                let selectedListItem = invoices |> List.tryFind (fun listItem -> listItem.InvoiceId = selectedInvoiceId)
                match selectedListItem with
                | Some selected -> AddDetailTab selected |> Cmd.ofMsg
                | None -> Cmd.none
            | None -> Cmd.none
        newState, cmd
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

        let newSelection =
            state.SelectedListItems |> List.filter (fun selected -> selected.InvoiceId <> invoice.InvoiceId)

        let newItems =
            state.ListItems |> List.filter (fun item -> item.InvoiceId <> invoice.InvoiceId)

        { state with SelectedListItems = newSelection; ListItems = newItems }, cmd
    | ListItemRemoved result ->
        match result with
        | Ok _ -> state, Cmd.none
        | Error DeleteInvoiceError.AuthorizationError ->
            state, showErrorToastCmd "U heeft niet genoeg rechten om een factuur te verwijderen"
        | Error DeleteInvoiceError.NotFound ->
            printf "The invoice that was being deleted was not found in the DB... Somehow..."
            state, showErrorToastCmd "De factuur werd niet gevonden in de databank"
    | RemotingError e ->
        state, showGenericErrorModalCmd e
    | Created invoice ->
        let listItem = toListItem invoice
        let newListItems = listItem :: state.ListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems }, showSuccessToastCmd "De factuur is aangemaakt"
    | Edited invoice ->
        let listItem = toListItem invoice
        let newListItems = state.ListItems |> List.map (fun li -> if li.InvoiceId = invoice.InvoiceId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.InvoiceId = invoice.InvoiceId then listItem else li)
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems }, showSuccessToastCmd "De factuur is gewijzigd"
    | FinancialYearsLoaded financialYears ->
        { state with FinancialYears = financialYears }, Cmd.none
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
            SortableTable.render 
                {|
                    ListItems = state.ListItems
                    DisplayAttributes = SortableInvoiceListItemAttribute.All
                    IsSelected = None
                    OnSelect = None
                    IsEditable = Some (fun li -> not li.FinancialYearIsClosed)
                    OnEdit = Some (AddDetailTab >> dispatch)
                    IsDeletable = Some (fun li -> not li.FinancialYearIsClosed)
                    OnDelete = Some (RemoveListItem >> dispatch)
                    Key = "InvoicesPageTable"
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
                            [ str selected.LocalInvoiceNumber ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe factuur" ]
                ]
            ]

            match state.SelectedTab with
            | List -> list state
            | Details listItem -> 
                InvoiceDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        CurrentBuilding = state.CurrentBuilding
                        Identifier = listItem.InvoiceId
                        IsNew = false
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
            | New ->
                InvoiceDetails.render 
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

let render (props: InvoicesPageProps) =
    React.elmishComponent ("InvoicesPage", init props, update, view)