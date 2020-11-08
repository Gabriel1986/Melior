module Client.Components.BankAccountEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Feliz.ElmishComponents

open Shared.Read
open Shared.Library
open Client.Library
open Client.IbanValidator
open Client.ClientStyle
open Client.ClientStyle.Helpers

type BankAccountEditComponentProps = 
    {|
        BankAccount: BankAccount
        OnChange: BankAccount -> unit
        OnDelete: (BankAccount -> unit) option
        BasePath: string
        Errors: (string * string) list
    |} 

type State = {
    BankAccount: BankAccount
    OnChange: BankAccount -> unit
    OnDelete: (BankAccount -> unit) option
    BasePath: string
    Errors: (string * string) list
    ValidatingIban: bool
}

type Msg =
    | IbanChanged of string
    | ValidateIban of string
    | IbanValidated of IbanValidationResult
    | ExceptionOccured of exn
    | NoOp

let init (props: BankAccountEditComponentProps) =
    {
        BankAccount = props.BankAccount
        OnChange = props.OnChange
        OnDelete = props.OnDelete
        BasePath = props.BasePath
        Errors = props.Errors
        ValidatingIban = false
    }, Cmd.none

let formatIban (iban: string) =
    if not (String.IsNullOrWhiteSpace iban) 
    then
        iban 
        |> String.filter Char.IsLetterOrDigit 
        |> String.chunk 4 
        |> String.joinWith " "
    else
        ""

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let changeBankAccount (bankAccount: BankAccount) (state: State) =
        { state with BankAccount = bankAccount }

    match msg with
    | IbanChanged newIban ->
        state
        |> changeBankAccount { state.BankAccount with IBAN = newIban; Validated = None }, Cmd.none
    | ValidateIban iban ->
        let cmd =
            Cmd.OfAsync.either
                Client.IbanValidator.validateIban iban
                IbanValidated
                ExceptionOccured
        { state with ValidatingIban = true }, cmd
    | IbanValidated result ->
        let state = { state with ValidatingIban = false }
        match result.valid, result.messages with
        | true, _ ->
            state.OnChange({ state.BankAccount with IBAN = formatIban state.BankAccount.IBAN; BIC = result.bankData.bic; Validated = Some true })
            state, showSuccessToastCmd "IBAN nummer gevalideerd, BIC werd automatisch ingevuld"
        | false, Some messages ->
            state.OnChange({ state.BankAccount with IBAN = formatIban state.BankAccount.IBAN; BIC = ""; Validated = Some false })
            state, showErrorToastCmd (messages |> String.joinWith "\r\n")
        | false, _ ->
            state.OnChange({ state.BankAccount with IBAN = formatIban state.BankAccount.IBAN; BIC = ""; Validated = Some false })
            state, showErrorToastCmd "Er is iets misgegaan bij het valideren van het IBAN nummer"
    | ExceptionOccured error ->
        state, showGenericErrorModalCmd error
    | NoOp ->
        state, Cmd.none

let private view (state: State) (dispatch: Msg -> unit) =
    let errorFor path = 
        state.Errors |> List.tryPick (fun (ePath, error) -> if ePath = (sprintf "%s.%s" state.BasePath path) then Some error else None)

    div [ classes [ Bootstrap.row; "full-width" ] ] [
        div [ Class Bootstrap.colMd ] [
            formGroup [
                Label "Naam rekening"
                Input [ 
                    Type "text"
                    MaxLength 255.0 
                    Helpers.valueOrDefault state.BankAccount.Description
                    OnChange (fun e -> { state.BankAccount with Description = e.Value } |> state.OnChange)
                ]
                FormError (errorFor (nameof state.BankAccount.Description))
            ]
        ]
        div [ Class Bootstrap.colMd ] [
            div [ Class Bootstrap.formGroup ] [
                label [ HtmlFor "IBAN" ] [ str "IBAN" ]

                div [ Class Bootstrap.inputGroup ] [
                    input [
                        Type "text"
                        Helpers.valueOrDefault state.BankAccount.IBAN
                        OnChange (fun e -> IbanChanged e.Value |> dispatch)
                    ]
                    div [ Class Bootstrap.inputGroupAppend ] [
                        match state.BankAccount.Validated with
                        | Some true ->
                            yield
                                button [
                                    Type "button"
                                    classes [ Bootstrap.btn; Bootstrap.btnSuccess ]
                                ] [ 
                                    i [ classes [ FontAwesome.fa; FontAwesome.faThumbsUp ] ] [] 
                                ]
                        | Some false ->
                            match state.ValidatingIban with
                            | true ->
                                yield
                                    button [ 
                                        Type "button"
                                        classes [ Bootstrap.btn; Bootstrap.btnPrimary ] 
                                    ] [
                                        i [ classes [ FontAwesome.fa; FontAwesome.faSpinner; FontAwesome.faSpin ] ] []
                                    ]
                            | false ->
                                yield
                                    button [
                                        Type "button"
                                        classes [ Bootstrap.btn; Bootstrap.btnDanger ]
                                    ] [
                                        i [ classes [ FontAwesome.fa; FontAwesome.faThumbsDown ] ] []
                                    ]
                        | None ->
                            yield
                                button [
                                    Type "button"
                                    classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
                                    OnClick (fun _ -> ValidateIban (state.BankAccount.IBAN) |> dispatch)
                                ] [
                                    i [ classes [ FontAwesome.fa; FontAwesome.faSearch ] ] []
                                    span [] [ str "Valideren" ]
                                ]
                    ]
                ]

                match errorFor (nameof state.BankAccount.IBAN) with
                | Some error -> div [ Class Bootstrap.invalidFeedback ] [ str error ]
                | None -> null
            ]
        ]
        div [ Class Bootstrap.colMd ] [
            formGroup [ 
                Label "BIC"
                Input [ 
                    Type "text"
                    MaxLength 12.0
                    Disabled true
                    Helpers.valueOrDefault state.BankAccount.BIC
                ] 
                FormError (errorFor (nameof state.BankAccount.BIC))
            ]
        ]
        match state.OnDelete with
        | Some onDelete ->
            div [ Class Bootstrap.colMd ] [
                div [] [
                    label [ Style [ Visibility "hidden" ]; classes [ Bootstrap.dNone; Bootstrap.dMdBlock ] ] [ str "_" ]
                    div [ Class Bootstrap.formGroup ] [
                        button [ 
                            classes [ Bootstrap.btn; Bootstrap.btnDanger ]
                            OnClick (fun _ -> onDelete state.BankAccount)
                        ] [
                            str "Verwijderen"
                        ]
                    ]
                ]
            ]
        | None ->
            null
    ]

let render =
    FunctionComponent.Of (
        fun props -> Feliz.React.elmishComponent("BankAccount", init props, update, view)
        , "BankAccountComponent"
        , memoEqualsButFunctions
        , fun props -> props.BankAccount.IBAN)
