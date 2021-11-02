module Client.Components.BankAccountEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React

open Shared.Read
open Shared.Library
open Client.Library
open Client.IbanValidator
open Client.ClientStyle
open Client.ClientStyle.Helpers

type EditComponentSettings =
    | Multiple of BankAccount list
    | Single of description: string option * BankAccount

type BankAccountEditComponentProps = 
    {|
        Settings: EditComponentSettings
        BasePath: string
    |} 

type State = {
    Settings: EditComponentSettings
    BankAccounts: BankAccount list
    BasePath: string
    ValidatingIban: bool
}

type Msg =
    | DescriptionChanged of int * string
    | IbanChanged of int * string
    | ValidateIban of int * string
    | IbanValidated of int * IbanValidationResult
    | BankAccountRemoved of int
    | BankAccountAdded
    | ExceptionOccured of exn
    | NoOp

let init (props: BankAccountEditComponentProps) =
    {
        Settings = props.Settings
        BankAccounts =
            match props.Settings with
            | Multiple bankAccounts -> bankAccounts
            | Single (Some description, bankAccount) -> [ { bankAccount with Description = description } ]
            | Single (None, bankAccount) -> [ bankAccount ]
        BasePath = props.BasePath
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
    let changeBankAccount (index: int) (apply: BankAccount -> BankAccount) (state: State) =
        { state with BankAccounts = state.BankAccounts |> List.mapi (fun i b -> if i = index then apply b else b) }

    match msg with
    | IbanChanged (index, newIban) ->
        state
        |> changeBankAccount index (fun bankAccount -> { bankAccount with IBAN = newIban; Validated = None }), Cmd.none
    | DescriptionChanged (index, newDescription) ->
        state
        |> changeBankAccount index (fun bankAccount -> { bankAccount with Description = newDescription }), Cmd.none
    | ValidateIban (index, iban) ->
        let cmd =
            Cmd.OfAsync.either
                Client.IbanValidator.validateIban iban
                (fun validated -> IbanValidated (index, validated))
                ExceptionOccured
        { state with ValidatingIban = true }, cmd
    | IbanValidated (index, result) ->
        let state = { state with ValidatingIban = false }
        match result.valid, result.messages with
        | true, _ ->
            state
            |> changeBankAccount index (fun bankAccount -> { 
                    bankAccount with 
                        IBAN = formatIban bankAccount.IBAN
                        BIC = result.bankData.bic
                        Validated = Some true 
                })
            , showSuccessToastCmd "IBAN nummer gevalideerd, BIC werd automatisch ingevuld"
        | false, Some messages ->
            state
            |> changeBankAccount index (fun bankAccount ->
                { bankAccount with 
                    IBAN = formatIban bankAccount.IBAN
                    BIC = ""
                    Validated = Some false 
                })
            , showErrorToastCmd (messages |> String.joinWith "\r\n")
        | false, _ ->
            state
            |> changeBankAccount index (fun bankAccount ->
                { bankAccount with 
                    IBAN = formatIban bankAccount.IBAN
                    BIC = ""
                    Validated = Some false 
                })
            , showErrorToastCmd "Er is iets misgegaan bij het valideren van het IBAN nummer"
    | ExceptionOccured error ->
        state, showGenericErrorModalCmd error
    | NoOp ->
        state, Cmd.none
    | BankAccountRemoved index ->
        let newBankAccounts =
            state.BankAccounts 
            |> List.indexed 
            |> List.choose (fun (i, bankAccount) -> if i = index then None else Some bankAccount)

        { state with BankAccounts = newBankAccounts }
        , Cmd.none
    | BankAccountAdded ->
        { state with BankAccounts = state.BankAccounts @ [ BankAccount.Init () ] }
        , Cmd.none

let view (errors: (string * string) list) (state: State) (dispatch: Msg -> unit) =
    let errorFor path index = 
        match state.Settings with
        | Single _ ->
            errors |> List.tryPick (fun (ePath, error) -> if ePath = (sprintf "%s.%s" state.BasePath path) then Some error else None)
        | Multiple _ ->
            errors |> List.tryPick (fun (ePath, error) -> if ePath = (sprintf "%s[%i].%s" state.BasePath index path) then Some error else None)

    [
        yield! state.BankAccounts 
        |> List.mapi (fun index bankAccount ->
            div [ classes [ Bootstrap.row; "full-width" ] ] [
                match state.Settings with
                | Single (Some _, _) ->
                    div [ Class Bootstrap.colMd ] [
                        formGroup [
                            Label "Naam rekening"
                            Input [ 
                                Type "text"
                                MaxLength 255.0 
                                Helpers.valueOrDefault bankAccount.Description
                                Disabled true
                            ]
                            FieldError (errorFor (nameof bankAccount.Description) index)
                        ]
                    ]
                | _ ->
                    div [ Class Bootstrap.colMd ] [
                        formGroup [
                            Label "Naam rekening"
                            Input [ 
                                Type "text"
                                MaxLength 255.0 
                                Helpers.valueOrDefault bankAccount.Description
                                OnChange (fun e -> DescriptionChanged (index, e.Value) |> dispatch)
                            ]
                            FieldError (errorFor (nameof bankAccount.Description) index)
                        ]
                    ]
                div [ Class Bootstrap.colMd ] [
                    formGroup [
                        Label "IBAN"
                        Input [
                            Type "text"
                            Helpers.valueOrDefault bankAccount.IBAN
                            OnChange (fun e -> IbanChanged (index, e.Value) |> dispatch)
                        ]
                        InputAppend [
                            match bankAccount.Validated with
                            | Some true ->
                                yield
                                    button [
                                        Type "button"
                                        classes [ Bootstrap.btn; Bootstrap.btnOutlineSuccess ]
                                    ] [ 
                                        i [ classes [ FontAwesome.fa; FontAwesome.faThumbsUp ] ] [] 
                                    ]
                            | Some false ->
                                match state.ValidatingIban with
                                | true ->
                                    yield
                                        button [ 
                                            Type "button"
                                            classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ] 
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
                                        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]
                                        OnClick (fun _ -> ValidateIban (index, bankAccount.IBAN) |> dispatch)
                                    ] [
                                        i [ classes [ FontAwesome.fa; FontAwesome.faSearch ] ] []
                                        str " "
                                        str "Valideren"
                                    ]
                        ]
                        FieldError (errorFor (nameof bankAccount.IBAN) index)
                    ]
                ]
                div [ Class Bootstrap.colMd ] [
                    formGroup [ 
                        Label "BIC"
                        Input [ 
                            Type "text"
                            MaxLength 12.0
                            Disabled true
                            Helpers.valueOrDefault bankAccount.BIC
                        ]
                        FieldError (errorFor (nameof bankAccount.BIC) index)
                    ]
                ]
                match state.Settings with
                | Single _ -> null
                | Multiple _ ->
                    div [ Class Bootstrap.colMd ] [
                        [
                            label [ Style [ Visibility "hidden" ]; classes [ Bootstrap.dNone; Bootstrap.dMdBlock ] ] [ str "_" ]
                            div [ Class Bootstrap.formGroup ] [
                                button [ 
                                    classes [ Bootstrap.btn; Bootstrap.btnOutlineDanger ]
                                    OnClick (fun _ -> BankAccountRemoved index |> dispatch)
                                ] [
                                    i [ classes [ FontAwesome.fa; FontAwesome.faTrashAlt ] ] []
                                    str " "
                                    str "Verwijderen"
                                ]
                            ]
                        ]
                        |> fragment []
                    ]
            ]
        )
        match state.Settings with
        | Single _ ->
            ()
        | Multiple _ -> 
            yield
                formGroup [
                    OtherChildren [
                        button [ 
                            classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]
                            OnClick (fun _ -> BankAccountAdded |> dispatch) 
                        ] [ 
                            i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                            str " "
                            str "Bankrekening toevoegen" 
                        ]
                    ]
                ]
    ]
    |> fragment []