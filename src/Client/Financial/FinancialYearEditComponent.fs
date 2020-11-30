module Client.Financial.FinancialYearEditComponent

open System
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Shared.Read
open Shared.Library
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Shared.Write

type FinancialYearEditComponentProps = {|
    FinancialYear: FinancialYear
    CreateOrUpdate: CreateOrUpdate
    Errors: (string * string) list
|}

type State = {
    FinancialYear: FinancialYear
    CreateOrUpdate: CreateOrUpdate
    Errors: (string * string) list
}

type Message =
    | CodeChanged of string
    | StartDateChanged of DateTime
    | EndDateChanged of DateTime

let initCreate (financialYear: FinancialYear) =
    {
        FinancialYear = financialYear
        CreateOrUpdate = Create
        Errors = []
    }, Cmd.none

let initUpdate (financialYear: FinancialYear) =
    {
        FinancialYear = financialYear
        CreateOrUpdate = Update
        Errors = []
    }, Cmd.none

let update (msg: Message) (state: State): State * Cmd<Message> =
    let recalculateValidationErrors (state: State) =
        match state.Errors with
        | [] -> state
        | _errors ->
            match ValidatedFinancialYear.Validate state.FinancialYear with
            | Ok _validated -> state
            | Error validationErrors -> { state with Errors = validationErrors }

    let updateFinancialYear updateFunction state: State =
        { state with FinancialYear = updateFunction state.FinancialYear }

    match msg with
    | CodeChanged newCode ->
        state |> updateFinancialYear (fun y -> { y with Code = newCode })
        , Cmd.none
    | StartDateChanged newStartDate ->
        state |> updateFinancialYear (fun y -> { y with StartDate = newStartDate })
        , Cmd.none
    | EndDateChanged newEndDate ->
        state |> updateFinancialYear (fun y -> { y with EndDate = newEndDate })
        , Cmd.none

    |> (fun (state, cmd) -> recalculateValidationErrors state, cmd)

let view (state: State) (dispatch: Message -> unit) =
    let errorFor (path: string) =
        printf "All errors: %A" state.Errors
        printf "Path: %s" path
        state.Errors |> List.tryPick (fun (p, error) -> if p = path then Some error else None)

    div [] [
        formGroup [
            Label "Code"
            Input [
                Type "text"
                valueOrDefault state.FinancialYear.Code
                OnChange (fun e -> CodeChanged e.Value |> dispatch)
            ]
            FieldError (errorFor (nameof state.FinancialYear.Code))
        ]
        div [ Style [ Width "120px" ] ] [
            formGroup [
                Label "Begindatum"
                Date [
                    Flatpickr.OnChange (fun e -> StartDateChanged e |> dispatch)
                    Flatpickr.Value (state.FinancialYear.StartDate)
                    Flatpickr.SelectionMode Flatpickr.Mode.Single
                    Flatpickr.EnableTimePicker false
                    Flatpickr.Locale Flatpickr.Locales.dutch
                    Flatpickr.DateFormat "d/m/Y"
                ]
                FieldError (errorFor (nameof state.FinancialYear.StartDate))
            ]
        ]
        div [ Style [ Width "120px" ] ] [
            formGroup [
                Label "Einddatum"
                Date [
                    Flatpickr.OnChange (fun e -> EndDateChanged e |> dispatch)
                    Flatpickr.Value (state.FinancialYear.EndDate)
                    Flatpickr.SelectionMode Flatpickr.Mode.Single
                    Flatpickr.EnableTimePicker false
                    Flatpickr.Locale Flatpickr.Locales.dutch
                    Flatpickr.DateFormat "d/m/Y"
                ]
                FieldError (errorFor (nameof state.FinancialYear.EndDate))
            ]
        ]
    ]