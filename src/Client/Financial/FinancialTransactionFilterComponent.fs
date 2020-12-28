module Client.Financial.FinancialTransactionFilterComponent

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

type PeriodType =
    | FinancialYearType
    | YearType
    | MonthType

type State = {
    BuildingId: BuildingId
    LoadingFinancialYears: bool
    FinancialYears: FinancialYear list
    FilterKey: string
    Filter: FinancialTransactionFilter
    Debouncer: Debouncer.State

    OnPrepareToChangeFilter: unit -> unit
    OnFilterChanged: FinancialTransactionFilter -> unit
    OnError: exn -> unit
    OnlyShowFinancialYears: bool
}

type Msg =
    | FinancialYearsLoaded of financialYears: FinancialYear list
    | PeriodTypeChanged of PeriodType
    | DebouncerSelfMsg of Debouncer.SelfMessage<Msg>
    | NotifyFilterChanged of FinancialTransactionFilter
    | FilterPeriodChanged of FinancialTransactionFilterPeriod
    | RemotingError of exn

module Server =
    let loadFinancialYears (buildingId: BuildingId) =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetFinancialYears)
            buildingId
            (fun years -> FinancialYearsLoaded years)
            RemotingError

let private getFilter (currentBuildingId: Guid, filterKey: string) (financialYears: FinancialYear list) =
    let invoiceFilter = Browser.Dom.window.localStorage.getItem(filterKey)
    let parsedInvoiceFilter =
        if String.IsNullOrWhiteSpace(invoiceFilter) then
            None
        else
            match Json.tryParseAs<FinancialTransactionFilter> invoiceFilter with
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

type FinancialTransactionFilterProps =
    {|
        CurrentBuildingId: BuildingId
        FilterKey: string
        OnFilterChanged: FinancialTransactionFilter -> unit
        OnPrepareToChangeFilter: unit -> unit
        OnError: exn -> unit
        OnlyShowFinancialYears: bool
    |}

let private init (props: FinancialTransactionFilterProps) =
    {
        BuildingId = props.CurrentBuildingId
        LoadingFinancialYears = true
        FinancialYears = []
        FilterKey = props.FilterKey
        Filter = { BuildingId = props.CurrentBuildingId; Period = Year 0 }
        Debouncer = Debouncer.create ()
        OnFilterChanged = props.OnFilterChanged
        OnPrepareToChangeFilter = props.OnPrepareToChangeFilter
        OnError = props.OnError
        OnlyShowFinancialYears = props.OnlyShowFinancialYears
    }, Server.loadFinancialYears props.CurrentBuildingId

   
let private update (msg: Msg) (state: State): State * Cmd<Msg> =
    let periodChanged (period: FinancialTransactionFilterPeriod) =
        if period = state.Filter.Period then
            state, Cmd.none
        else
            let newFilter = { state.Filter with Period = period }
            Browser.Dom.window.localStorage.setItem(state.FilterKey, SimpleJson.Json.stringify(newFilter))
    
            let newDebouncerState, newDebouncerCmd =
                Debouncer.create ()
                |> Debouncer.bounce (TimeSpan.FromSeconds 1.0) (sprintf "filter::%A" newFilter) (NotifyFilterChanged newFilter)

            state.OnPrepareToChangeFilter ()
            { state with Filter = newFilter; Debouncer = newDebouncerState }
            , newDebouncerCmd |> Cmd.map DebouncerSelfMsg
    
    match msg with
    | FinancialYearsLoaded financialYears ->
        let sortedFinancialYears = financialYears |> List.sortBy (fun fy -> fy.EndDate)
        let filter = getFilter (state.BuildingId, state.FilterKey) sortedFinancialYears

        state.OnFilterChanged (filter)
        { state with FinancialYears = sortedFinancialYears; Filter = filter }
        , Cmd.ofMsg (NotifyFilterChanged filter)

    | PeriodTypeChanged periodType ->
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
    | NotifyFilterChanged filter ->
        state.OnFilterChanged filter
        state, Cmd.none
    | DebouncerSelfMsg debouncerMsg ->
        let (debouncerModel, debouncerCmd) = Debouncer.update debouncerMsg state.Debouncer
        { state with Debouncer = debouncerModel }, debouncerCmd
    | FilterPeriodChanged filterPeriod ->
        periodChanged filterPeriod
    | RemotingError error ->
        state.OnError error
        state, Cmd.none

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

let private periodTypesToOptions (period: FinancialTransactionFilterPeriod) (onPeriodTypeChanged: PeriodType -> unit): FormRadioButton list = [
    {
        Id = "Month"
        Key = "Month"
        Label = "Maand"
        IsSelected = match period with | Month _ -> true | _ -> false
        OnClick = fun _ -> onPeriodTypeChanged (MonthType)
    }
    {
        Id = "Year"
        Key = "Year"
        Label = "Jaar"
        IsSelected = match period with | Year _ -> true | _ -> false
        OnClick = fun _ ->  onPeriodTypeChanged(YearType)
    }
    {
        Id = "FinancialYear"
        Key = "FinancialYear"
        Label = "Boekjaar"
        IsSelected = match period with | FinancialYear _ -> true | _ -> false
        OnClick = fun _ -> onPeriodTypeChanged(FinancialYearType)
    }
]

let private months =
    [ "Januari"; "Februari"; "Maart"; "April"; "Mei"; "Juni"; "Juli"; "Augustus"; "September"; "Oktober"; "November"; "December" ]

let private view (state: State) (dispatch: Msg -> unit): ReactElement =
    div [ classes [ Bootstrap.card; Bootstrap.textCenter ] ] [
        div [ Class Bootstrap.cardHeader ] [
            printf "only show financial years %A" state.OnlyShowFinancialYears
            if not state.OnlyShowFinancialYears then
                formGroup [
                    Label "Periode"
                    Radio {
                        Inline = true
                        RadioButtons = periodTypesToOptions state.Filter.Period (fun newType -> Msg.PeriodTypeChanged newType |> dispatch)
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
                    | _ -> leftChevronBtn (fun _ -> FilterPeriodChanged (FinancialYear (state.FinancialYears.[matchingIndex-1].FinancialYearId)) |> dispatch)
                    span [ 
                        classes [ Bootstrap.cardText; "pointer" ]
                        OnClick (fun _ -> PeriodTypeChanged FinancialYearType |> dispatch) 
                    ] [ str matchingFinancialYear.Code ]
                    match matchingIndex with
                    | x when x = (state.FinancialYears.Length - 1) -> null
                    | _ -> rightChevronBtn (fun _ -> FilterPeriodChanged (FinancialYear (state.FinancialYears.[matchingIndex+1].FinancialYearId)) |> dispatch)
                ]
            | Year _ when state.OnlyShowFinancialYears ->
                div [] [ str "Gelieve een boekjaar te starten" ]
            | Year year ->
                div [ Style [ Width "200px" ]; Class Bootstrap.dInlineBlock ] [
                    leftChevronBtn (fun _ -> FilterPeriodChanged (Year (year-1)) |> dispatch)
                    span [ 
                        classes [ Bootstrap.cardText; "pointer" ]
                        OnClick (fun _ -> FilterPeriodChanged (Year today.Year) |> dispatch) 
                    ] [ str (string year) ]
                    rightChevronBtn (fun _ -> FilterPeriodChanged (Year (year+1)) |> dispatch)
                ]
            | Month _ when state.OnlyShowFinancialYears ->
                div [] [ str "Gelieve een boekjaar te starten" ]                
            | Month (month, year) ->
                div [ Style [ Width "200px" ]; classes [ Bootstrap.dInlineBlock ] ] [
                    leftChevronBtn (fun _ -> FilterPeriodChanged (Month ((if month = 1 then 12 else month-1), year)) |> dispatch)
                    span [
                        classes [ Bootstrap.cardText; "pointer" ]
                        OnClick (fun _ -> 
                            FilterPeriodChanged (FinancialTransactionFilterPeriod.Month (today.Month, year))
                            |> dispatch) 
                    ] [ str (months.[month-1]) ]
                    rightChevronBtn (fun _ -> FilterPeriodChanged (Month ((if month = 12 then 1 else month+1), year)) |> dispatch)
                ]
                div [ Style [ Width "200px" ]; classes [ Bootstrap.dInlineBlock; Bootstrap.ml1 ] ] [
                    leftChevronBtn (fun _ -> FilterPeriodChanged (Month (month, year-1)) |> dispatch)
                    span [ 
                        classes [ Bootstrap.cardText; "pointer" ]
                        OnClick (fun _ ->
                            FilterPeriodChanged (FinancialTransactionFilterPeriod.Month (month, today.Year))
                            |> dispatch) 
                    ] [ str (string year) ]
                    rightChevronBtn (fun _ -> FilterPeriodChanged (Month (month, year+1)) |> dispatch)
                ]
        ]
    ]

let render =
    FunctionComponent.Of (
        (fun props -> React.elmishComponent ("FinancialTransactionFilter", init props, update, view)),
        "FinancialTransactionFilterComponent",
        memoEqualsButFunctions)