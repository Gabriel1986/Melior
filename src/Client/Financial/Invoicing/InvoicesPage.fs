module Client.Financial.Invoicing.InvoicesPage

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Fable.SimpleJson
open Feliz
open Feliz.ElmishComponents
open Thoth.Elmish

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
    SelectedListItems: InvoiceListItem list
    SelectedTab: Tab
    LoadingFilters: bool
    LoadingListItems: bool
    ListItems: InvoiceListItem list
    LoadingFinancialYears: bool
    FinancialYears: FinancialYear list
    Filter: InvoiceFilter
    Debouncer: Debouncer.State
}
and Tab =
    | List
    | Details of invoiceId: Guid
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
    | InvoiceFilterPeriodChanged of InvoiceFilterPeriod
    | InvoiceFilterPeriodTypeChanged of PeriodType
    | GetInvoices of InvoiceFilter
    | NoOp
    | DebouncerSelfMsg  of Debouncer.SelfMessage<Msg>

and PeriodType =
    | FinancialYearType
    | YearType
    | MonthType

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
    override me.ToString () =
        match me with
        | InvoiceNumber -> "Nr."
        | Organization -> "Leverancier"
        | DistributionKey -> "Sleutel"
        | Cost -> "Totaal"
        | DueDate -> "Betaaldag"
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
        member me.ReactElementFor = me.ReactElementFor'
        member _.ExtraHeaderAttributes = Seq.empty
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true


let getInvoices (filter: InvoiceFilter, selectedTab: Tab) =
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
        ListItems = []
        LoadingFilters = true
        LoadingListItems = true
        FinancialYears = []
        LoadingFinancialYears = true
        Filter = { 
            BuildingId = props.CurrentBuilding.BuildingId
            Period = Year (DateTime.Now.Year) 
        }
        Debouncer = Debouncer.create ()
    }

    let cmd =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetFinancialYears)
            props.CurrentBuilding.BuildingId
            (fun years -> FinancialYearsLoaded years)
            RemotingError
    state, cmd

let private getInvoiceFilter (currentBuildingId: Guid, financialYears: FinancialYear list) =
    let invoiceFilter = Browser.Dom.window.localStorage.getItem("InvoiceFilter")
    let parsedInvoiceFilter =
        if String.IsNullOrWhiteSpace(invoiceFilter) then
            None
        else
            match Json.tryParseAs<InvoiceFilter> invoiceFilter with
            | Ok invoiceFilter -> 
                Some invoiceFilter
            | Error e ->
                Browser.Dom.console.error("Failed to parse invoice filter: ", e)
                None
    match parsedInvoiceFilter with
    | Some invoiceFilter when invoiceFilter.BuildingId = currentBuildingId ->
        invoiceFilter
    | Some _
    | None ->
        {
            BuildingId = currentBuildingId
            Period =
                match financialYears |> List.tryHead with
                | Some financialYear -> FinancialYear financialYear.FinancialYearId 
                | None -> Year DateTime.Now.Year
        }

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
    }

    let periodChanged (period: InvoiceFilterPeriod) =
        if period = state.Filter.Period then
            state, Cmd.none
        else
            let newFilter = { state.Filter with Period = period }
            Browser.Dom.window.localStorage.setItem("InvoiceFilter", SimpleJson.Json.stringify(newFilter))

            let newDebouncerState, newDebouncerCmd =
                Debouncer.create ()
                |> Debouncer.bounce (TimeSpan.FromSeconds 1.0) (sprintf "filter::%A" newFilter) (GetInvoices newFilter)

            { state with Filter = newFilter; Debouncer = newDebouncerState; LoadingListItems = true }
            , newDebouncerCmd |> Cmd.map DebouncerSelfMsg

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
        let newState = { state with ListItems = invoices; LoadingFilters = false; LoadingListItems = false }
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
        { state with ListItems = []; LoadingListItems = false; LoadingFilters = false }, showGenericErrorModalCmd e
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
        let sortedFinancialYears = financialYears |> List.sortBy (fun fy -> fy.EndDate)
        let filter = getInvoiceFilter (state.CurrentBuilding.BuildingId, sortedFinancialYears)

        { state with FinancialYears = sortedFinancialYears; Filter = filter }
        , getInvoices (filter, state.SelectedTab)
    | InvoiceFilterPeriodChanged period ->
        periodChanged period
    | GetInvoices filter ->
        { state with LoadingFilters = true }, getInvoices (filter, state.SelectedTab)
    | InvoiceFilterPeriodTypeChanged periodType ->
        let newFilterPeriodResult =
            match periodType with
            | PeriodType.FinancialYearType ->
                match state.FinancialYears with
                | [] -> Error "Gelieve eerst een boekjaar in te geven bij de instellingen"
                | financialYears -> Ok (FinancialYear (financialYears.Head.FinancialYearId))
            | PeriodType.YearType ->
                match state.Filter.Period with
                | Year year ->
                    Ok (Year year)
                | Month (_month, year) ->
                    Ok (Year year)
                | _ ->
                    Ok (Year DateTime.Now.Year)
            | PeriodType.MonthType ->
                let now = DateTime.Now
                match state.Filter.Period with
                | Year year ->
                    Ok (Month (now.Month, year))
                | Month (month, year) ->
                    Ok (Month (month, year))
                | _ ->
                    Ok (Month (now.Month, now.Year))

        match newFilterPeriodResult with
        | Ok newPeriod ->
            periodChanged newPeriod
        | Error e ->
            state, showErrorToastCmd e
    | NoOp ->
        state, Cmd.none
    | DebouncerSelfMsg debouncerMsg ->
        let (debouncerModel, debouncerCmd) = Debouncer.update debouncerMsg state.Debouncer
        { state with Debouncer = debouncerModel }, debouncerCmd

let private leftChevronBtn (onClick: unit -> unit) =
    button [
        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary; Bootstrap.btnSm; Bootstrap.floatLeft ]
        OnClick (fun _ -> onClick ())
    ] [
        i [ classes [ FontAwesome.fa; FontAwesome.faChevronLeft ] ] []
    ]

let private rightChevronBtn (onClick: unit -> unit) =
    button [
        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary; Bootstrap.btnSm; Bootstrap.floatRight ]
        OnClick (fun _ -> onClick ())
    ] [
        i [ classes [ FontAwesome.fa; FontAwesome.faChevronRight ] ] []
    ]

let private periodFilterPeriodTypes (state: State) (dispatch: Msg -> unit): FormRadioButton list = [
    {
        Id = "Month"
        Key = "Month"
        Label = "Maand"
        IsSelected = match state.Filter.Period with | Month _ -> true | _ -> false
        OnClick = fun _ -> InvoiceFilterPeriodTypeChanged MonthType |> dispatch
    }
    {
        Id = "Year"
        Key = "Year"
        Label = "Jaar"
        IsSelected = match state.Filter.Period with | Year _ -> true | _ -> false
        OnClick = fun _ ->  InvoiceFilterPeriodTypeChanged YearType |> dispatch
    }
    {
        Id = "FinancialYear"
        Key = "FinancialYear"
        Label = "Boekjaar"
        IsSelected = match state.Filter.Period with | FinancialYear _ -> true | _ -> false
        OnClick = fun _ -> InvoiceFilterPeriodTypeChanged FinancialYearType |> dispatch
    }
]

let private months =
    [ "Januari"; "Februari"; "Maart"; "April"; "Mei"; "Juni"; "Juli"; "Augustus"; "September"; "Oktober"; "November"; "December" ]

let private renderPeriodFilter (state: State) (dispatch: Msg -> unit): ReactElement =
    fieldset [ Disabled state.LoadingFilters ] [
        div [ classes [ Bootstrap.card; Bootstrap.textCenter ] ] [
            div [ Class Bootstrap.cardHeader ] [
                formGroup [
                    Label "Periode"
                    Radio {
                        Inline = true
                        RadioButtons = periodFilterPeriodTypes state dispatch
                    }
                ]
                let today = DateTime.Now

                match state.Filter.Period with
                | FinancialYear financialYearId ->
                    let matchingIndex, matchingFinancialYear = 
                        state.FinancialYears
                        |> List.indexed
                        |> List.find (fun (_, fy) -> fy.FinancialYearId = financialYearId)
                    div [ Style [ Width "200px" ]; Class Bootstrap.dInlineBlock ] [
                        match matchingIndex with
                        | 0 -> null
                        | _ -> leftChevronBtn (fun _ -> InvoiceFilterPeriodChanged (FinancialYear (state.FinancialYears.[matchingIndex-1].FinancialYearId)) |> dispatch)
                        span [ 
                            classes [ Bootstrap.cardText; "pointer" ]
                            OnClick (fun _ -> InvoiceFilterPeriodTypeChanged FinancialYearType |> dispatch) 
                        ] [ str matchingFinancialYear.Code ]
                        match matchingIndex with
                        | x when x = (state.FinancialYears.Length - 1) -> null
                        | _ -> rightChevronBtn (fun _ -> InvoiceFilterPeriodChanged (FinancialYear (state.FinancialYears.[matchingIndex+1].FinancialYearId)) |> dispatch)
                    ]
                | Year year ->
                    div [ Style [ Width "200px" ]; Class Bootstrap.dInlineBlock ] [
                        leftChevronBtn (fun _ -> InvoiceFilterPeriodChanged (Year (year-1)) |> dispatch)
                        span [ 
                            classes [ Bootstrap.cardText; "pointer" ]
                            OnClick (fun _ -> InvoiceFilterPeriodChanged (Year today.Year) |> dispatch) 
                        ] [ str (string year) ]
                        rightChevronBtn (fun _ -> InvoiceFilterPeriodChanged (Year (year+1)) |> dispatch)
                    ]
                | Month (month, year) ->
                    div [ Style [ Width "200px" ]; classes [ Bootstrap.dInlineBlock ] ] [
                        leftChevronBtn (fun _ -> InvoiceFilterPeriodChanged (Month ((if month = 1 then 12 else month-1), year)) |> dispatch)
                        span [
                            classes [ Bootstrap.cardText; "pointer" ]
                            OnClick (fun _ -> 
                                InvoiceFilterPeriodChanged (InvoiceFilterPeriod.Month (today.Month, year))
                                |> dispatch) 
                        ] [ str (months.[month-1]) ]
                        rightChevronBtn (fun _ -> InvoiceFilterPeriodChanged (Month ((if month = 12 then 1 else month+1), year)) |> dispatch)
                    ]
                    div [ Style [ Width "200px" ]; classes [ Bootstrap.dInlineBlock; Bootstrap.ml1 ] ] [
                        leftChevronBtn (fun _ -> InvoiceFilterPeriodChanged (Month (month, year-1)) |> dispatch)
                        span [ 
                            classes [ Bootstrap.cardText; "pointer" ]
                            OnClick (fun _ ->
                                InvoiceFilterPeriodChanged (InvoiceFilterPeriod.Month (month, today.Year))
                                |> dispatch) 
                        ] [ str (string year) ]
                        rightChevronBtn (fun _ -> InvoiceFilterPeriodChanged (Month (month, year+1)) |> dispatch)
                    ]
            ]
        ]
    ]

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
                renderPeriodFilter state dispatch
                SortableTable.render 
                    {|
                        ListItems = if state.LoadingListItems then [] else state.ListItems
                        DisplayAttributes = SortableInvoiceListItemAttribute.All
                        IsSelected = None
                        OnSelect = None
                        IsEditable = Some (fun li -> not li.FinancialYearIsClosed)
                        OnEdit = Some (AddDetailTab >> dispatch)
                        IsDeletable = Some (fun li -> not li.FinancialYearIsClosed)
                        OnDelete = Some (RemoveListItem >> dispatch)
                        Key = "InvoicesPageTable"
                    |}
                if state.LoadingListItems then
                    div [ Class Bootstrap.textCenter ] [
                        str "Facturen worden geladen..."
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
    ]

let render (props: InvoicesPageProps) =
    React.elmishComponent ("InvoicesPage", init props, update, view)