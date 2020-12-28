module Client.Financial.FinancialTransactions.Balance

open System
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents
open Elmish
open Shared.Read
open Client
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Library
open Client.Routing
open Client.SortableTable

type State = {
    CurrentBuilding: BuildingListItem
    ListItemsState: ListItemsState
}
and ListItemsState =
    | PreparingToLoad
    | Loading
    | Loaded of FinancialTransaction list
    member me.ListItems =
        match me with
        | PreparingToLoad
        | Loading -> []
        | Loaded listItems -> listItems

type SortableFinancialTransactionListItemAttribute =
    | Source
    | Date
    | FinancialCategoryCode
    | FinancialCategoryDescription
    | DebitAmount
    | CreditAmount
    override me.ToString () =
        match me with
        | Source -> "Details"
        | Date -> "Datum"
        | FinancialCategoryCode -> "Rekeningnummer"
        | FinancialCategoryDescription -> "Naam rekening"
        | DebitAmount -> "Debet"
        | CreditAmount -> "Credit"
    member me.ReactElementFor': FinancialTransaction -> ReactElement =
        match me with
        | Source -> (fun li ->
            let page =
                match li.Source with
                | Some (FinancialTransactionSource.Invoice invoice) -> 
                    Page.InvoiceDetails { BuildingId = li.BuildingId; DetailId = invoice.InvoiceId }
                    |> Some
                | Some (FinancialTransactionSource.InvoicePayment payment) -> 
                    Page.InvoiceDetails { BuildingId = li.BuildingId; DetailId = payment.InvoiceId }
                    |> Some
                | None ->
                    None
            match page with
            | Some page ->
                span [] [
                    str (me.StringValueOf' li)
                    |> wrapInLink page
                ]
            | None ->
                null)
        | x -> (fun li -> str (x.StringValueOf' li))
    member me.StringValueOf': FinancialTransaction -> string =
        match me with
        | Source -> (fun li ->
            match li.Source with
            | Some (FinancialTransactionSource.Invoice invoice) ->
                sprintf "Factuur voor %s" invoice.OrganizationName
            | Some (FinancialTransactionSource.InvoicePayment payment) ->
                sprintf "Betaling voor %s" payment.OrganizationName
            | None ->
                "")
        | Date ->
            fun li -> li.Date.ToString("dd/MM/yyyy")
        | FinancialCategoryCode -> 
            fun li -> li.FinancialCategoryCode
        | FinancialCategoryDescription -> 
            fun li -> li.FinancialCategoryDescription
        | DebitAmount ->
            fun li ->
                match li.Amount with
                | Debit amount -> String.Format("€{0:0.00}", amount).Replace('.', ',')
                | Credit _ -> ""
        | CreditAmount ->
            fun li ->
                match li.Amount with
                | Debit _ -> ""
                | Credit amount -> String.Format("€{0:0.00}", amount).Replace('.', ',')
    member me.Compare': FinancialTransaction -> FinancialTransaction -> int =
        match me with
        | Date ->
            fun li otherLi -> li.Date.CompareTo(otherLi.Date)
        | CreditAmount ->
            fun li otherLi -> 
                match li.Amount, otherLi.Amount with
                | Credit amount, Credit otherAmount -> int (amount - otherAmount)
                | Credit _, Debit _ -> 1
                | Debit _, Credit _ -> -1
                | Debit amount, Debit otherAmount -> int (amount - otherAmount) 
        | DebitAmount ->
            fun li otherLi ->
                match li.Amount, otherLi.Amount with
                | Credit amount, Credit otherAmount -> int (amount - otherAmount)
                | Credit _, Debit _ -> -1
                | Debit _, Credit _ -> 1
                | Debit amount, Debit otherAmount -> int (amount - otherAmount) 
        | _ ->
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ Date; FinancialCategoryCode; FinancialCategoryDescription; DebitAmount; CreditAmount; Source ]
    interface ISortableAttribute<FinancialTransaction> with
        member me.ReactElementFor = me.ReactElementFor'
        member _.ExtraHeaderAttributes = Seq.empty
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true

type Msg =
    | GetFinancialTransactions of FinancialTransactionFilter
    | FinancialTransactionsLoaded of FinancialTransaction list
    | ChangeListItemsState of ListItemsState
    | RemotingExceptionOccured of exn

type BalanceProps =
    {|
        CurrentBuilding: BuildingListItem
    |}

module Server =
    let getRemotingApi () = Client.Remoting.getRemotingApi()

    let loadFinancialTransactions (filter: FinancialTransactionFilter) =
        Cmd.OfAsync.either
            (getRemotingApi()).GetFinancialTransactions filter
            FinancialTransactionsLoaded
            RemotingExceptionOccured

let private init (props: BalanceProps) =
    {
        CurrentBuilding = props.CurrentBuilding
        ListItemsState = Loading
    }, Cmd.none

let private update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | FinancialTransactionsLoaded financialTransactions ->
        { state with ListItemsState = Loaded financialTransactions }
        , Cmd.none
    | RemotingExceptionOccured error ->
        state, showGenericErrorModalCmd error
    | GetFinancialTransactions filter ->
        { state with ListItemsState = Loading }, Server.loadFinancialTransactions filter
    | ChangeListItemsState newState ->
        { state with ListItemsState = newState }, Cmd.none

let private createBalance (transactions: FinancialTransaction list) =
    transactions 
    |> List.groupBy (fun li -> li.FinancialCategoryCode)
    |> List.map (fun (code, transactions) ->
        let transaction = transactions.Head
        tr [] [
            td [] [
                str transaction.FinancialCategoryCode
            ]
            td [] [
                str transaction.FinancialCategoryDescription
            ]
            td [] [
                transactions
                |> List.sumBy (fun li -> match li.Amount with | Debit amount-> amount | _ -> Decimal.Zero)
                |> (fun amount -> String.Format("€{0:0.00}", amount).Replace('.', ','))
                |> str
            ]
            td [] [
                transactions
                |> List.sumBy (fun li -> match li.Amount with | Credit amount -> amount | _ -> Decimal.Zero)
                |> (fun amount -> String.Format("€{0:0.00}", amount).Replace('.', ','))
                |> str
            ]
            td [] [
                let balancedResult =
                    transactions
                    |> List.sumBy (fun li-> match li.Amount with | Debit amount -> amount | Credit amount -> Decimal.MinusOne * amount)

                if balancedResult < Decimal.Zero then
                    str "€0,00"
                else
                    str (String.Format("€{0:0.00}", balancedResult).Replace('.', ','))
            ]
            td [] [
                let balancedResult =
                    transactions
                    |> List.sumBy (fun li -> match li.Amount with | Credit amount -> amount | Debit amount -> Decimal.MinusOne * amount)

                if balancedResult < Decimal.Zero then
                    str "€0,00"
                else
                    str (String.Format("€{0:0.00}", balancedResult).Replace('.', ','))
            ]
        ]
    )

let private view (state: State) (dispatch: Msg -> unit) =
    div [] [
        let filterIsDisabled =
            match state.ListItemsState with
            | Loading -> true
            | _ -> false
        fieldset [ Disabled filterIsDisabled ] [
            Client.Financial.FinancialTransactionFilterComponent.render
                {|
                    CurrentBuildingId = state.CurrentBuilding.BuildingId
                    FilterKey = "FinancialTransactionsFilter"
                    OnFilterChanged = GetFinancialTransactions >> dispatch
                    OnPrepareToChangeFilter = (fun _ -> ChangeListItemsState PreparingToLoad |> dispatch)
                    OnError = RemotingExceptionOccured >> dispatch
                    OnlyShowFinancialYears = true
                |}
        ]
        match state.ListItemsState with
        | PreparingToLoad
        | Loading ->
            div [] [
                str "Bezig met laden"
            ]
        | Loaded listItems ->
            div [] [
                table [ classes [ Bootstrap.table; Bootstrap.tableHover; Bootstrap.tableBordered ] ] [
                    thead [] [
                        tr [] [
                            th [] [
                                str "Rek. nr."
                            ]
                            th [] [
                                str "Naam rekening"
                            ]
                            th [] [
                                str "Debet"
                            ]
                            th [] [
                                str "Credit"
                            ]
                            th [] [
                                str "Debet-saldo"
                            ]
                            th [] [
                                str "Credit-saldo"
                            ]
                        ]
                    ]
                    tbody [] [
                        let filteredItems =
                            listItems
                            |> List.filter (fun li -> [ "1"; "2"; "3"; "4"; "5" ] |> List.exists (fun code -> li.FinancialCategoryCode.StartsWith(code)))
                    
                        yield! createBalance filteredItems
                        yield tr [ Style [ TextDecorationLine "underline" ] ] [
                            td [ ColSpan 2 ] [
                                str "Totaal klasse 1 t.e.m. 5"
                            ]
                            td [] [
                                filteredItems 
                                |> List.sumBy (fun li -> match li.Amount with | Debit amount-> amount | _ -> Decimal.Zero)
                                |> (fun amount -> String.Format("€{0:0.00}", amount).Replace('.', ','))
                                |> str
                            ]
                            td [] [
                                filteredItems 
                                |> List.sumBy (fun li -> match li.Amount with | Credit amount-> amount | _ -> Decimal.Zero)
                                |> (fun amount -> String.Format("€{0:0.00}", amount).Replace('.', ','))
                                |> str
                            ]

                            td [] [
                                let balancedResult =
                                    filteredItems
                                    |> List.groupBy (fun li -> li.FinancialCategoryCode)
                                    |> List.sumBy (fun (_, listItems) ->
                                        let sum = listItems |> List.sumBy (fun li -> match li.Amount with | Debit amount -> amount | Credit amount -> Decimal.MinusOne * amount)
                                        if sum < Decimal.Zero then Decimal.Zero else sum)

                                if balancedResult < Decimal.Zero then
                                    str "€0,00"
                                else
                                    str (String.Format("€{0:0.00}", balancedResult).Replace('.', ','))
                            ]
                            td [] [
                                let balancedResult =
                                    filteredItems
                                    |> List.groupBy (fun li -> li.FinancialCategoryCode)
                                    |> List.sumBy (fun (_, listItems) ->
                                        let sum = listItems |> List.sumBy (fun li -> match li.Amount with | Credit amount -> amount | Debit amount -> Decimal.MinusOne * amount)
                                        if sum < Decimal.Zero then Decimal.Zero else sum)

                                if balancedResult < Decimal.Zero then
                                    str "€0,00"
                                else
                                    str (String.Format("€{0:0.00}", balancedResult).Replace('.', ','))
                            ]
                        ]

                        let filteredItems =
                            listItems
                            |> List.filter (fun li -> [ "6"; "7" ] |> List.exists (fun code -> li.FinancialCategoryCode.StartsWith(code)))

                        yield! createBalance filteredItems
                        yield tr [ Style [ TextDecorationLine "underline" ] ] [
                            td [ ColSpan 2 ] [
                                str "Totaal klasse 6 en 7"
                            ]
                            td [] [
                                filteredItems 
                                |> List.sumBy (fun li -> match li.Amount with | Debit amount-> amount | _ -> Decimal.Zero)
                                |> (fun amount -> String.Format("€{0:0.00}", amount).Replace('.', ','))
                                |> str
                            ]
                            td [] [
                                filteredItems 
                                |> List.sumBy (fun li -> match li.Amount with | Credit amount-> amount | _ -> Decimal.Zero)
                                |> (fun amount -> String.Format("€{0:0.00}", amount).Replace('.', ','))
                                |> str
                            ]

                            td [] [
                                let balancedResult =
                                    filteredItems
                                    |> List.groupBy (fun li -> li.FinancialCategoryCode)
                                    |> List.sumBy (fun (_, listItems) ->
                                        let sum = listItems |> List.sumBy (fun li -> match li.Amount with | Debit amount -> amount | Credit amount -> Decimal.MinusOne * amount)
                                        if sum < Decimal.Zero then Decimal.Zero else sum)

                                if balancedResult < Decimal.Zero then
                                    str "€0,00"
                                else
                                    str (String.Format("€{0:0.00}", balancedResult).Replace('.', ','))
                            ]
                            td [] [
                                let balancedResult =
                                    filteredItems
                                    |> List.groupBy (fun li -> li.FinancialCategoryCode)
                                    |> List.sumBy (fun (_, listItems) ->
                                        let sum = listItems |> List.sumBy (fun li -> match li.Amount with | Credit amount -> amount | Debit amount -> Decimal.MinusOne * amount)
                                        if sum < Decimal.Zero then Decimal.Zero else sum)

                                if balancedResult < Decimal.Zero then
                                    str "€0,00"
                                else
                                    str (String.Format("€{0:0.00}", balancedResult).Replace('.', ','))
                            ]
                        ]
                        yield tr [ Class Bootstrap.fontWeightBold ] [
                            td [ ColSpan 2 ] [
                                str "TOTAAL"
                            ]
                            td [] [
                                listItems 
                                |> List.sumBy (fun li -> match li.Amount with | Debit amount-> amount | _ -> Decimal.Zero)
                                |> (fun amount -> String.Format("€{0:0.00}", amount).Replace('.', ','))
                                |> str
                            ]
                            td [] [
                                listItems 
                                |> List.sumBy (fun li -> match li.Amount with | Credit amount-> amount | _ -> Decimal.Zero)
                                |> (fun amount -> String.Format("€{0:0.00}", amount).Replace('.', ','))
                                |> str
                            ]

                            td [] [
                                let balancedResult =
                                    listItems
                                    |> List.groupBy (fun li -> li.FinancialCategoryCode)
                                    |> List.sumBy (fun (_, listItems) ->
                                        let sum = listItems |> List.sumBy (fun li -> match li.Amount with | Debit amount -> amount | Credit amount -> Decimal.MinusOne * amount)
                                        if sum < Decimal.Zero then Decimal.Zero else sum)

                                if balancedResult < Decimal.Zero then
                                    str "€0,00"
                                else
                                    str (String.Format("€{0:0.00}", balancedResult).Replace('.', ','))
                            ]
                            td [] [
                                let balancedResult =
                                    listItems
                                    |> List.groupBy (fun li -> li.FinancialCategoryCode)
                                    |> List.sumBy (fun (_, listItems) ->
                                        let sum = listItems |> List.sumBy (fun li -> match li.Amount with | Credit amount -> amount | Debit amount -> Decimal.MinusOne * amount)
                                        if sum < Decimal.Zero then Decimal.Zero else sum)

                                if balancedResult < Decimal.Zero then
                                    str "€0,00"
                                else
                                    str (String.Format("€{0:0.00}", balancedResult).Replace('.', ','))
                            ]
                        ]
                    ]
                ]
            ]
    ]

let render (props: BalanceProps) =
    React.elmishComponent ("FinancialTransactionsPage", init props, update, view)