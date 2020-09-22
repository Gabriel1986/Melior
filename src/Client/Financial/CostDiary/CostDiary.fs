module Client.Financial.CostDiary.CostDiaryPage

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
    CurrentBuildingId: Guid
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
    | Loaded of listItems: InvoiceListItem list
    | FinancialYearsLoaded of financialYears: FinancialYear list
    | RemoveListItem of InvoiceListItem
    | ListItemRemoved of Result<InvoiceListItem, DeleteInvoiceError>
    | Created of Invoice
    | Edited of Invoice

type CostDiaryProps = {|
    CurrentUser: User
    CurrentBuildingId: Guid
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
//CategoryCode: string //Rubriek
//DueDate: DateTimeOffset
//HasLinkedDocuments: bool
//HasBeenPaid: bool

type SortableInvoiceListItemAttribute =
    | InvoiceNumber
    | Category
    | Organization
    | DistributionKey
    | Cost
    | VatRate
    | DueDate
    | HasBeenPaid
    member me.ToString' () =
        match me with
        | InvoiceNumber -> "Nr."
        | Category -> "Rubriek"
        | Organization -> "Leverancier"
        | DistributionKey -> "Verdeelsleutel"
        | Cost -> "Totaal"
        | VatRate -> "%BTW"
        | DueDate -> "Betaaldag"
        | HasBeenPaid -> "Betaald"
    member me.StringValueOf': InvoiceListItem -> string =
        match me with
        | InvoiceNumber -> (fun li -> li.LocalInvoiceNumber)
        | Category -> (fun li -> li.CategoryCode)
        | Organization -> (fun li -> li.OrganizationName)
        | DistributionKey -> (fun li -> li.DistributionKeyName)
        | Cost -> (fun li -> string li.Cost)
        | VatRate -> (fun li -> string li.VatRate)
        | DueDate -> (fun li -> sprintf "%02i-%02i-%i" li.DueDate.Day li.DueDate.Month li.DueDate.Year)
        | HasBeenPaid -> (fun li -> if li.HasBeenPaid then "Ja" else "Nee")
    member me.Compare': InvoiceListItem -> InvoiceListItem -> int =
        match me with
        | DueDate -> 
            fun li otherLi -> li.DueDate.CompareTo(otherLi.DueDate)
        | Cost ->
            fun li otherLi -> li.Cost.CompareTo(otherLi.Cost)
        | VatRate ->
            fun li otherLi -> li.Cost.CompareTo(otherLi.VatRate)
        | _     -> 
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ InvoiceNumber; Category; Organization; DistributionKey; Cost; VatRate; DueDate; HasBeenPaid ]
    interface ISortableAttribute<InvoiceListItem> with
        member me.ToString = me.ToString'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true

let init (props: CostDiaryProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        CurrentBuildingId = props.CurrentBuildingId
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
            props.CurrentBuildingId
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
        Cost = invoice.Cost
        VatRate = invoice.VatRate
        DistributionKeyName = invoice.DistributionKey.Name
        OrganizationName = invoice.Organization.Name
        CategoryCode = invoice.CategoryCode
        DueDate = invoice.DueDate
        HasBeenPaid = not invoice.PaymentIds.IsEmpty
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
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Cmd.none
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
            state, Cmd.none
    | RemotingError e ->
        state, showGenericErrorModalCmd e
    | Created invoice ->
        let listItem = toListItem invoice
        let newListItems = listItem :: state.ListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems }, Cmd.none
    | Edited invoice ->
        let listItem = toListItem invoice
        let newListItems = state.ListItems |> List.map (fun li -> if li.InvoiceId = invoice.InvoiceId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.InvoiceId = invoice.InvoiceId then listItem else li)
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
                        [ str "Nieuwe kavel" ]
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

let render (props: CostDiaryProps) =
    React.elmishComponent ("CostDiaryPage", init props, update, view)