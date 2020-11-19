module Client.Contracts.ContractsPage

open System
open Elmish
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents
open Thoth.Elmish

open Shared.Read
open Shared.MediaLibrary
open Shared.Remoting
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Library
open Client.Components
open Client.Components.BasicModal
open Client.Upload
open Client.Routing
open Client.Contracts.Translations

type State = {
    CurrentBuildingId: BuildingId
    CurrentUser: User
    IsLoading: bool
    ContractTypeAnswers: Savable<ContractTypeAnswer> list
    Contracts: Contract list
    ContractModalIsOpenOn: (Contract * bool) option
    QuestionModalIsOpen: bool
    Debouncer: Debouncer.State
}

type Message =
    | ContractsRetrieved of Contract list
    | ContractTypeAnswersRetrieved of ContractTypeAnswer list
    | SetAnswer of ContractTypeQuestion * isTrue: bool
    | SaveAnswer of Savable<ContractTypeAnswer>
    | AnswerSaved of Result<ContractTypeAnswer, SaveAnswerError>
    | ShowOrganizationDetails of organizationId: Guid
    | CreateNewContract
    | CreateMandatoryContract of PredefinedContractType
    | EditContract of Contract
    | DeleteContract of Contract
    | ContractDeleted of Result<unit, DeleteContractError>
    | CloseContractModal
    | SaveContract of Contract * isNew: bool
    | ContractSaved of Result<unit, SaveContractError>
    | DebouncerSelfMsg  of Debouncer.SelfMessage<Message>
    | RemotingException of exn
    | OpenQuestionModal
    | CloseQuestionModal

type ContractsPageProps = 
    {|
        CurrentBuildingId: Guid
        CurrentUser: User
    |}

//TODO: get contracts, get answers
let init (props: ContractsPageProps) =
    let cmd1 =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).GetContracts props.CurrentBuildingId
            ContractsRetrieved
            RemotingException

    let cmd2 =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).GetContractTypeAnswers props.CurrentBuildingId
            ContractTypeAnswersRetrieved
            RemotingException

    { 
        CurrentBuildingId = props.CurrentBuildingId
        CurrentUser = props.CurrentUser 
        IsLoading = true
        ContractTypeAnswers = []
        Contracts = []
        ContractModalIsOpenOn = None
        QuestionModalIsOpen = false
        Debouncer = Debouncer.create()
    }, Cmd.batch [ cmd1; cmd2 ]

let private unansweredQuestionsExist (answeredQuestions: ContractTypeAnswer list): bool =
    let allQuestions =
        ContractTypeQuestion.AllValues () 
        |> Set.ofArray

    let answeredQuestions =
        answeredQuestions
        |> List.map (fun a -> a.Question)
        |> Set.ofList

    (allQuestions - answeredQuestions) |> Set.count > 0 

let update (msg: Message) (state: State): State * Cmd<Message> =
    match msg with
    | ContractsRetrieved contracts ->
        { state with Contracts = contracts }, Cmd.none
    | ContractTypeAnswersRetrieved answers ->
        let cmd =
            if unansweredQuestionsExist answers
            then Cmd.ofMsg OpenQuestionModal
            else Cmd.none
        { state with ContractTypeAnswers = answers |> List.map (fun a -> { IsSaving = false; Payload = a  }) }, cmd
    | DebouncerSelfMsg debouncerMsg ->
        let (debouncerModel, debouncerCmd) = Debouncer.update debouncerMsg state.Debouncer
        { state with Debouncer = debouncerModel }, debouncerCmd
    | SetAnswer (question, isTrue) ->
        let answer: Savable<ContractTypeAnswer> = {
            IsSaving = false
            Payload = {
                BuildingId = state.CurrentBuildingId
                Question = question
                IsTrue = isTrue
            }
        }

        let (debouncerModel, debouncerCmd) =
            Debouncer.create ()
            |> Debouncer.bounce (TimeSpan.FromSeconds 2.0) (sprintf "answer_to_%A" question) (SaveAnswer answer)

        let newAnswers = answer::(state.ContractTypeAnswers |> List.filter (fun a -> a.Payload.Question <> answer.Payload.Question))

        { state with ContractTypeAnswers = newAnswers;  Debouncer = debouncerModel }, Cmd.map DebouncerSelfMsg debouncerCmd

    | SaveAnswer (savable) ->
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).SaveContractTypeAnswer (savable.Payload)
                (fun result -> result |> Result.map (fun _ -> savable.Payload) |> AnswerSaved)
                RemotingException
        let newAnswers = state.ContractTypeAnswers |> List.map (fun a -> if a = savable then { a with IsSaving = true } else a)

        { state with ContractTypeAnswers = newAnswers }, cmd
    | AnswerSaved (Ok answer) ->
        //No need to show a toast here, let the user assume it worked.
        let newAnswers = state.ContractTypeAnswers |> List.map (fun a -> if a.Payload = answer then { a with IsSaving = false } else a)
        { state with ContractTypeAnswers = newAnswers }, Cmd.none
    | AnswerSaved (Error e) ->
        match e with
        | SaveAnswerError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om een antwoord te geven op de vragen"
        | SaveAnswerError.NotFound ->
            printf "We should not be getting this error -> Could not save the answer, the answer was not found? O_o"
            state, Cmd.none
    
    | ShowOrganizationDetails organizationId ->
        state, Client.Routing.navigateToPage (Page.OrganizationDetails { BuildingId = state.CurrentBuildingId; DetailId = organizationId })
    | CreateNewContract ->
        let contract: Contract = {
            ContractId = Guid.NewGuid()
            BuildingId = state.CurrentBuildingId
            ContractType = OtherContractType ""
            ContractOrganization = None
            ContractFile = None
        }
        { state with ContractModalIsOpenOn = Some (contract, true) }, Cmd.none
    | CreateMandatoryContract predefinedType ->
        let contract: Contract = {
            ContractId = Guid.NewGuid()
            BuildingId = state.CurrentBuildingId
            ContractType = PredefinedContractType predefinedType
            ContractOrganization = None
            ContractFile = None
        }
        { state with ContractModalIsOpenOn = Some (contract, true) }, Cmd.none
    | EditContract contract ->
        { state with ContractModalIsOpenOn = Some (contract, false) }, Cmd.none
    | DeleteContract contract ->
        let newContracts = state.Contracts |> List.filter (fun c -> c.ContractId <> contract.ContractId)
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).DeleteContract (state.CurrentBuildingId, contract.ContractId)
                ContractDeleted
                RemotingException
        { state with Contracts = newContracts }, cmd
    | ContractDeleted (Ok ()) ->
        state, showSuccessToastCmd "Contract verwijderd"
    | ContractDeleted (Error e) ->
        match e with
        | DeleteContractError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om een contract te verwijderen..."
        | DeleteContractError.NotFound ->
            printf "The contract that was being deleted is not found in the DB, somehow? O_o"
            state, Cmd.none
    | CloseContractModal ->
        { state with ContractModalIsOpenOn = None }, Cmd.none
    | SaveContract (c, isNew) ->
        let saveMethod, newContracts =
            if isNew 
            then
                (Client.Remoting.getRemotingApi()).CreateContract, 
                c::state.Contracts
            else
                (Client.Remoting.getRemotingApi()).UpdateContract, 
                state.Contracts |> List.map (fun existing -> if existing.ContractId = c.ContractId then c else existing)

        let cmd =
            Cmd.OfAsync.either
                saveMethod c
                ContractSaved
                RemotingException

        { state with Contracts = newContracts; ContractModalIsOpenOn = None }, cmd
    | ContractSaved (Ok ()) ->
        state, showSuccessToastCmd "Uw contract is bewaard"
    | ContractSaved (Error e) ->
        match e with
        | SaveContractError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om een contract te bewaren"
        | SaveContractError.NotFound ->
            state, showErrorToastCmd "Het contract dat u probeerde te bewaren is reeds verwijderd"
        | SaveContractError.Validation (errors) ->
            state, showErrorToastCmd "Er is iets misgelopen bij de validatie van het contract dat u probeerde te bewaren"
    | RemotingException e ->
        printf "Error details: %A" e
        state, showErrorToastCmd "Er is iets misgelopen bij het bewaren van uw gegevens. Gelieve de pagina te verversen."
    | OpenQuestionModal ->
        { state with QuestionModalIsOpen = true }, Cmd.none
    | CloseQuestionModal ->
        { state with QuestionModalIsOpen = false }, Cmd.none
        
let view (state: State) (dispatch: Message -> unit) =
    let renderQuestion (question: ContractTypeQuestion) =
        let answer = state.ContractTypeAnswers |> List.tryFind (fun answer -> answer.Payload.Question = question)
        let btnStyle = Style [ MinWidth "60px" ]
        let btnClasses = [ Bootstrap.btn; Bootstrap.btnSm; Bootstrap.ml2 ]

        tr [] [
            yield td [] [ str (translateQuestion question) ]

            match answer with
            | Some savable ->
                let positiveClass = if savable.Payload.IsTrue then Bootstrap.btnSecondary else Bootstrap.btnOutlineSecondary
                let negativeClass = if savable.Payload.IsTrue then Bootstrap.btnOutlineSecondary else Bootstrap.btnSecondary
                
                if savable.IsSaving then
                    yield td [ Class Bootstrap.textRight ] [
                        button [ Type "button"; btnStyle; classes (positiveClass::btnClasses); HTMLAttr.Disabled true ] [ str "Ja" ]
                        button [ Type "button"; btnStyle; classes (negativeClass::btnClasses); HTMLAttr.Disabled true ] [ str "Nee" ]
                    ]
                else
                    yield td [ Class Bootstrap.textRight ] [
                        button [ Type "button"; btnStyle; classes (positiveClass::btnClasses); OnClick (fun _ -> SetAnswer (question, true) |> dispatch) ] [ str "Ja" ]
                        button [ Type "button"; btnStyle; classes (negativeClass::btnClasses); OnClick (fun _ -> SetAnswer (question, false) |> dispatch) ] [ str "Nee" ]                    
                    ]
            | None ->
                yield td [ Class Bootstrap.textRight ] [
                    button [ Type "button"; btnStyle; classes (Bootstrap.btnOutlineSecondary::btnClasses); OnClick (fun _ -> SetAnswer (question, true) |> dispatch) ] [ str "Ja" ]
                    button [ Type "button"; btnStyle; classes (Bootstrap.btnOutlineSecondary::btnClasses); OnClick (fun _ -> SetAnswer (question, false) |> dispatch) ] [ str "Nee" ]                                    
                ]
        ]

    let renderQuestions (questions: ContractTypeQuestion array) =
        table [ classes [ Bootstrap.table ] ] [
            tbody [] [
                for question in questions do
                    yield renderQuestion question
            ]
        ]

    let translateContractType (c: Contract) =
        match c.ContractType with
        | PredefinedContractType predefined -> translatePredefinedType predefined
        | OtherContractType name -> name

    let rowsForContract (isMandatory: bool) (c: Contract)  =
        tr [] [
            td [] [ str (translateContractType c) ]
            td [] [ 
                match c.ContractFile with 
                | Some mediaFile -> 
                    span [] [
                        str mediaFile.FileName
                    ]
                    str " "
                    span [ HTMLAttr.Title "Downloaden" ] [
                        a [ Target "_blank"; Href (downloadUri Partitions.Contracts mediaFile.FileId) ] [
                            span [ classes [ FontAwesome.fas; FontAwesome.faCloudDownloadAlt ] ] []
                        ]
                    ]
                | None -> str "Geen bestand" ]
            td [] [ 
                match c.ContractOrganization with 
                | Some org -> 
                    span [] [
                        str org.Name
                        |> wrapInLink (Page.OrganizationDetails { BuildingId = state.CurrentBuildingId; DetailId = org.OrganizationId })
                    ]
                | None ->
                    null
            ]
            td [] [
                a [ Class "pointer"; OnClick (fun _ -> EditContract c |> dispatch) ] [
                    span [ classes [ FontAwesome.fas; FontAwesome.faEdit; Bootstrap.textPrimary ] ] []
                ]
            ]
            td [] [
                a [ classes [ "pointer"; Bootstrap.textDanger ]; OnClick (fun _ -> DeleteContract c |> dispatch) ] [
                    span [ classes [ FontAwesome.fas; FontAwesome.faTrashAlt ] ] []
                ]
            ]
        ]

    let rowsForPredefinedContractType (predefinedContractTypeIsMandatory: bool) (predefined: PredefinedContractType)  =
        let contracts = state.Contracts |> List.filter (fun contract -> contract.ContractType = PredefinedContractType predefined)
        match contracts with
        | [] ->
            [
                tr [] [
                    td [] [ str (translatePredefinedType predefined ) ]
                    td [] [ str "Geen bestand" ]
                    td [] []
                    if (predefinedContractTypeIsMandatory) 
                    then
                        td [ HTMLAttr.Title "Wettelijk verplicht"; Class Bootstrap.textDanger ] [ 
                            span [ classes [ FontAwesome.fas; FontAwesome.faExclamationTriangle ] ] []
                        ]
                    else
                        td [] []
                    td [] [
                        a [ classes [ "pointer"; Bootstrap.textPrimary ]; OnClick (fun _ -> CreateMandatoryContract predefined |> dispatch) ] [
                            span [ classes [ FontAwesome.fas; FontAwesome.faCloudUploadAlt ] ] []
                        ]
                    ]
                ]
            ]
        | contracts ->
            contracts |> List.map (rowsForContract true)

    div [ Class Bootstrap.row ] [
        table [ classes [ Bootstrap.table; Bootstrap.tableHover; Bootstrap.tableStriped ] ] [
            thead [] [
                tr [] [
                    th [] [ str "Naam" ]
                    th [] [ str "Bestand" ]
                    th [] [ str "Leverancier" ]
                    th [] []
                    th [] []
                ]
            ]
            tbody [] [
                yield! 
                    MandatoryContractTypes  
                    |> Array.toList
                    |> List.collect (rowsForPredefinedContractType true)

                yield!
                    state.ContractTypeAnswers 
                    |> List.collect (fun answer ->
                        mandatoryContractTypesFor answer.Payload
                        |> Array.toList
                        |> List.collect (rowsForPredefinedContractType true)
                    )

                yield!
                    OtherPredefinedContractTypes
                    |> Array.toList
                    |> List.collect (rowsForPredefinedContractType false)

                yield!
                    state.Contracts
                    |> List.choose (fun c -> 
                        match c.ContractType with 
                        | ContractContractType.OtherContractType _ -> Some (rowsForContract false c) 
                        | _ -> None)
            ]
        ]
        div [ classes [ Bootstrap.card; Bootstrap.bgLight; Bootstrap.dInlineBlock ] ] [
            div [ Class Bootstrap.cardBody ] [
                button [ classes [ Bootstrap.btn; Bootstrap.btnPrimary ]; Type "button"; OnClick (fun _ -> CreateNewContract |> dispatch) ] [
                    i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                    str " "
                    str "Contract aanmaken"
                ]
                button [ classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary; Bootstrap.ml2 ]; Type "button"; OnClick (fun _ -> OpenQuestionModal |> dispatch) ] [
                    i [ classes [ FontAwesome.fa; FontAwesome.faEdit ] ] []
                    str " "
                    str "Instellingen gebouw aanpassen"
                ]
            ]
        ]

        match state.ContractModalIsOpenOn with
        | Some x ->
            ContractModal.render
                {|
                    IsOpenOn = state.ContractModalIsOpenOn
                    BuildingId = state.CurrentBuildingId
                    OnOk = (fun (contract, isNew) -> SaveContract (contract, isNew) |> dispatch)
                    OnCanceled = (fun _ -> Message.CloseContractModal |> dispatch)
                |}
        | None ->
            ()

        BasicModal.render 
            {|
                ModalProps = [
                    IsOpen state.QuestionModalIsOpen
                    OnDismiss (fun () -> CloseQuestionModal |> dispatch)
                    DisableBackgroundClick false
                    Header [ BasicModal.HeaderProp.Title "Instellingen van het gebouw" ]
                    Body [ renderQuestions (ContractTypeQuestion.AllValues ()) ]
                    Footer [ 
                        FooterProp.Buttons [
                            button [ 
                                Type "button"
                                classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
                                OnClick (fun _ -> CloseQuestionModal |> dispatch)
                            ] [ 
                                str "Ok" 
                            ]
                        ]
                    ]
                    ModalSize SmallSize
                ]
            |}
    ]
    |> withPageHeader "Contracten"

let render (props: ContractsPageProps) =
    React.elmishComponent ("ContractsPage", init props, update, view)