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

type BankAccountEditComponentProps = 
    {|
        CurrentBuildingId: Guid option
        BankAccounts: BankAccount list
        BasePath: string
        ShowFinancialCategorySelection: bool
    |} 

type State = {
    BankAccounts: BankAccount list
    BasePath: string
    ValidatingIban: bool
    FinancialCategoriesLoaded: bool
    FinancialCategories: FinancialCategory list
    ShowFinancialCategorySelection: bool
    ShowingFinancialCategoryModalOn: int option
}

type Msg =
    | DescriptionChanged of int * string
    | IbanChanged of int * string
    | ValidateIban of int * string
    | IbanValidated of int * IbanValidationResult
    | BankAccountRemoved of int
    | BankAccountAdded
    | ExceptionOccured of exn
    | ChangeCategory of int
    | CategoryChanged of (int * FinancialCategory option)
    | CancelChangeFinancialCategory
    | FinancialCategoriesLoaded of FinancialCategory list
    | NoOp

module Server =
    let getFinancialCategories (buildingId: BuildingId) =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).GetFinancialCategories 
                buildingId
                FinancialCategoriesLoaded
                ExceptionOccured

let init (props: BankAccountEditComponentProps) =
    {
        BankAccounts = props.BankAccounts
        BasePath = props.BasePath
        ValidatingIban = false
        FinancialCategoriesLoaded = false
        FinancialCategories = []
        ShowFinancialCategorySelection = props.ShowFinancialCategorySelection
        ShowingFinancialCategoryModalOn = None
    },
        match props.CurrentBuildingId with
        | Some buildingId when props.ShowFinancialCategorySelection -> Server.getFinancialCategories buildingId
        | _ -> Cmd.none

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
        { state with BankAccounts = (state.BankAccounts |> List.mapi (fun i b -> if i = index then apply b else b)) }

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
    | ChangeCategory index ->
        { state with ShowingFinancialCategoryModalOn = Some index }, Cmd.none
    | CategoryChanged (index, financialCategory) ->
        let updatedBankAccounts =
            state.BankAccounts 
            |> List.mapi (fun i bankAccount -> if i = index then { bankAccount with FinancialCategoryId = financialCategory |> Option.map (fun cat -> cat.FinancialCategoryId) } else bankAccount)
        { state with ShowingFinancialCategoryModalOn = None; BankAccounts = updatedBankAccounts }
        , Cmd.none
    | CancelChangeFinancialCategory ->
        { state with ShowingFinancialCategoryModalOn = None }, Cmd.none
    | FinancialCategoriesLoaded financialCategories ->
        { state with FinancialCategories = financialCategories; FinancialCategoriesLoaded = true }, Cmd.none 



let view (errors: (string * string) list) (state: State) (dispatch: Msg -> unit) =
    let errorFor path index = 
        errors |> List.tryPick (fun (ePath, error) -> if ePath = (sprintf "%s[%i].%s" state.BasePath index path) then Some error else None)

    [
        yield! state.BankAccounts 
        |> List.mapi (fun index bankAccount ->
            div [ classes [ Bootstrap.row; "full-width" ] ] [
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
                if state.ShowFinancialCategorySelection && state.FinancialCategoriesLoaded then
                    let currentFinancialCategory = 
                        bankAccount.FinancialCategoryId 
                        |> Option.bind (fun catId -> state.FinancialCategories |> List.tryFind (fun cat -> cat.FinancialCategoryId = catId))
                    div [ Class Bootstrap.colMd ] [
                        formGroup [
                            Label "Boekhoudkundige rekening"

                            let shown =
                                match currentFinancialCategory with
                                | Some category -> sprintf "%s - %s" category.Code category.Description
                                | _ -> ""
                            Input [
                                Type "text"
                                Style [ Cursor "pointer"; BackgroundColor "unset" ]
                                ReadOnly true
                                OnClick (fun _ -> dispatch (ChangeCategory index))
                                valueOrDefault shown
                            ]
                            InputAppend [
                                button [ 
                                    classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]
                                    OnClick (fun _ -> dispatch (ChangeCategory index ))
                                ] [
                                    span [ classes [ FontAwesome.fas; FontAwesome.faSearch ] ] []
                                ]
                            ]
                            FieldError (errorFor (nameof bankAccount.FinancialCategoryId) index)
                        ]
                    ]
                else
                    null
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
        match state.ShowingFinancialCategoryModalOn with
        | None -> ()
        | Some index ->
            yield
                Client.Components.FinancialCategorySelectionModal.render
                    {|
                        SelectedCategoryId = (state.BankAccounts.[index]).FinancialCategoryId
                        FinancialCategories = state.FinancialCategories
                        Showing = true
                        OnCancelSelection = fun () -> dispatch CancelChangeFinancialCategory
                        OnSelectionChanged = fun cat -> dispatch (CategoryChanged (index, cat))
                        Filter = Some (fun cat -> cat.Code.StartsWith("55"))
                    |}
    ]
    |> fragment []