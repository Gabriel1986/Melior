﻿module Client.Contracts.ContractsPage

open System
open Elmish
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

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
    ContractTypeAnswers: ContractTypeAnswer list
    Contracts: Contract list
    ContractModalIsOpenOn: (Contract * bool) option
    QuestionModalIsOpen: bool
    Warnings: Warning list

    OnAnswersChanged: ContractTypeAnswer list -> unit
    OnContractsChanged: Contract list -> unit
}

type Message =
    | ContractsRetrieved of Contract list
    | ContractTypeAnswersRetrieved of ContractTypeAnswer list
    | SaveAnswers of ContractTypeAnswer list
    | AnswersSaved of Result<ContractTypeAnswer list, SaveAnswerError>
    | ShowOrganizationDetails of organizationId: Guid
    | CreateNewContract
    | CreateNewInsuranceContract
    | CreateMandatoryContract of PredefinedContractType
    | EditContract of Contract
    | DeleteContract of Contract
    | ConfirmDeleteContract of Contract
    | ContractDeleted of Result<unit, DeleteContractError>
    | CloseContractModal
    | SaveContract of Contract * isNew: bool
    | ContractSaved of Result<unit, SaveContractError>
    | RemotingException of exn
    | OpenQuestionModal
    | CloseQuestionModal
    | NoOp

type ContractsPageProps = 
    {|
        CurrentBuildingId: Guid
        CurrentUser: User
        Warnings: Warning list
        OnAnswersChanged: ContractTypeAnswer list -> unit
        OnContractsChanged: Contract list -> unit
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
        Warnings = props.Warnings
        OnAnswersChanged = props.OnAnswersChanged
        OnContractsChanged = props.OnContractsChanged
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

let private createContract (buildingId: Guid) (contractType: ContractType) = {
    ContractId = Guid.NewGuid()
    BuildingId = buildingId
    ContractType = contractType
    ContractOrganization = None
    ContractFiles = []
}

let update (msg: Message) (state: State): State * Cmd<Message> =
    let createContract = createContract state.CurrentBuildingId
    match msg with
    | ContractsRetrieved contracts ->
        { state with Contracts = contracts }, Cmd.none
    | ContractTypeAnswersRetrieved answers ->
        let cmd =
            if unansweredQuestionsExist answers
            then Cmd.ofMsg OpenQuestionModal
            else Cmd.none
        { state with ContractTypeAnswers = answers }, cmd

    | SaveAnswers answers ->
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).SetContractTypeAnswers answers
                (Result.map (fun () -> answers) >> AnswersSaved)
                RemotingException
        { state with ContractTypeAnswers = answers; QuestionModalIsOpen = false }, cmd
    | AnswersSaved (Ok answers) ->
        state.OnAnswersChanged answers
        state, showSuccessToastCmd "De instellingen werden bewaard"
    | AnswersSaved (Error e) ->
        match e with
        | SaveAnswerError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om een antwoord te geven op de vragen"
    | ShowOrganizationDetails organizationId ->
        state, Client.Routing.navigateToPage (Page.OrganizationDetails { BuildingId = state.CurrentBuildingId; DetailId = organizationId })
    | CreateNewContract ->
        { state with ContractModalIsOpenOn = Some (createContract (Other ""), true) }, Cmd.none
    | CreateMandatoryContract predefinedType ->
        { state with ContractModalIsOpenOn = Some (createContract (Predefined { Type = predefinedType; Broker = None }), true) }, Cmd.none
    | CreateNewInsuranceContract ->
        { state with ContractModalIsOpenOn = Some (createContract (Insurance { Name = ""; Broker = None }), true) }, Cmd.none
    | EditContract contract ->
        { state with ContractModalIsOpenOn = Some (contract, false) }, Cmd.none
    | DeleteContract contract ->
        state
        , showConfirmationModal
            {|
                Title = "Contract verwijderen?"
                Message =
                    sprintf
                        "Bent u er zeker van dat u %s wilt verwijderen?"
                        (contract.ContractType.Translate(Translations.translatePredefinedType))
                OnConfirmed = fun () -> ConfirmDeleteContract contract
                OnDismissed = fun () -> NoOp
            |}
    | ConfirmDeleteContract contract ->
        let newContracts = state.Contracts |> List.filter (fun c -> c.ContractId <> contract.ContractId)
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).DeleteContract (state.CurrentBuildingId, contract.ContractId)
                ContractDeleted
                RemotingException
        { state with Contracts = newContracts }, cmd
    | ContractDeleted (Ok ()) ->
        state.OnContractsChanged state.Contracts
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
        state.OnContractsChanged state.Contracts
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
    | NoOp ->
        state, Cmd.none

type QuestionProps =
    {|
        BuildingId: Guid
        Questions: ContractTypeQuestion array
        Answers: ContractTypeAnswer list
        IsOpen: bool
        Dispatch: Message -> unit
    |}

let private renderQuestion (question: ContractTypeQuestion) (answer: ContractTypeAnswer option) (updateAnswer: (ContractTypeQuestion * bool) -> unit) =        
    let btnStyle = Style [ MinWidth "60px" ]
    let btnClasses = [ Bootstrap.btn; Bootstrap.btnSm; Bootstrap.ml2 ]

    tr [] [
        yield td [] [ str (translateQuestion question) ]

        match answer with
        | Some answer ->
            let positiveClass = if answer.IsTrue then Bootstrap.btnSecondary else Bootstrap.btnOutlineSecondary
            let negativeClass = if answer.IsTrue then Bootstrap.btnOutlineSecondary else Bootstrap.btnSecondary
            
            yield td [ Class Bootstrap.textRight ] [
                button [ Type "button"; btnStyle; classes (positiveClass::btnClasses); OnClick (fun _ -> (question, true) |> updateAnswer) ] [ str "Ja" ]
                button [ Type "button"; btnStyle; classes (negativeClass::btnClasses); OnClick (fun _ -> (question, false) |> updateAnswer) ] [ str "Nee" ]                    
            ]
        | None ->
            yield td [ Class Bootstrap.textRight ] [
                button [ Type "button"; btnStyle; classes (Bootstrap.btnOutlineSecondary::btnClasses); OnClick (fun _ -> (question, true) |> updateAnswer) ] [ str "Ja" ]
                button [ Type "button"; btnStyle; classes (Bootstrap.btnOutlineSecondary::btnClasses); OnClick (fun _ -> (question, false) |> updateAnswer) ] [ str "Nee" ]                                    
            ]
    ]

let private renderQuestions (questions: ContractTypeQuestion array) (answers: ContractTypeAnswer list) (onUpdateAnswer: (ContractTypeQuestion * bool) -> unit) =
    let answerToQuestion question =
        answers |> List.tryFind (fun answer -> answer.Question = question)

    table [ classes [ Bootstrap.table ] ] [
        tbody [] [
            for question in questions do
                yield renderQuestion question (answerToQuestion question) onUpdateAnswer
        ]
    ]

let private renderQuestionsModal (props: QuestionProps) =
    let answersState = Hooks.useState props.Answers

    let addOrUpdateAnswer (question: ContractTypeQuestion, isTrue: bool) =
        if answersState.current |> List.exists (fun existing -> existing.Question = question) then
            answersState.current 
            |> List.map (fun existing -> if existing.Question = question then { existing with IsTrue = isTrue } else existing)
        else
            { Question = question; IsTrue = isTrue; BuildingId = props.BuildingId }::answersState.current

    BasicModal.render 
        {|
            ModalProps = [
                IsOpen props.IsOpen
                OnDismiss (fun () -> props.Dispatch CloseQuestionModal)
                DisableBackgroundClick false
                Header [ BasicModal.HeaderProp.Title "Instellingen van het gebouw" ]
                Body [ 
                    renderQuestions 
                        props.Questions 
                        answersState.current 
                        (fun answer -> answersState.update (addOrUpdateAnswer answer)) 
                ]
                Footer [
                    FooterProp.ShowDismissButton (Some "Annuleren")
                    FooterProp.Buttons [                            
                        button [ 
                            Type "button"
                            classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
                            OnClick (fun _ -> SaveAnswers answersState.current |> props.Dispatch)
                        ] [
                            i [ classes [ FontAwesome.fa; FontAwesome.faSave ] ] []
                            str " "
                            str "Bewaren" 
                        ]
                    ]
                ]
                ModalSize SmallSize
            ]
        |}

let private renderQuestionsModalComponent (state: State) (dispatch: Message -> unit) =
    FunctionComponent.Of((fun props -> renderQuestionsModal props), "QuestionsComponent", equalsButFunctions)
        {|
            BuildingId = state.CurrentBuildingId
            Questions = ContractTypeQuestion.AllValues ()
            Answers = state.ContractTypeAnswers
            IsOpen = state.QuestionModalIsOpen
            Dispatch = dispatch
        |}

let view (state: State) (dispatch: Message -> unit) =
    let translateContractType (c: Contract) =
        match c.ContractType with
        | Predefined predefined -> translatePredefinedType predefined.Type
        | Other name -> name
        | Insurance insurance -> insurance.Name

    let rowsForContract (isMandatory: bool) (c: Contract)  =
        tr [] [
            td [] [ str (translateContractType c) ]
            td [] [ 
                match c.ContractFiles with 
                | [] -> yield str "Geen bestand"
                | mediaFiles ->
                    yield! mediaFiles |> List.map (fun mediaFile ->
                        [
                            span [] [
                                str mediaFile.FileName
                            ]
                            str " "
                            span [ HTMLAttr.Title "Downloaden" ] [
                                a [ Target "_blank"; Href (downloadUri Partitions.Contracts mediaFile.FileId) ] [
                                    span [ classes [ FontAwesome.fas; FontAwesome.faCloudDownloadAlt ] ] []
                                ]
                            ]
                        ]
                        |> fragment []
                    )
            ]
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
                match c.ContractType with
                | Insurance { Broker = Some broker }
                | Predefined { Broker = Some broker } ->
                    span [] [
                        str broker.Name
                        |> wrapInLink (Page.OrganizationDetails { BuildingId = state.CurrentBuildingId; DetailId = broker.OrganizationId })
                    ]
                | _ ->
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
        let contracts = state.Contracts |> List.filter (fun contract -> match contract.ContractType with | Predefined { Type = x } -> x = predefined | _ -> false)
        match contracts with
        | [] ->
            [
                tr [] [
                    td [] [ str (translatePredefinedType predefined ) ]
                    td [] [ str "Geen bestand" ]
                    td [] []
                    td [] []
                    if predefinedContractTypeIsMandatory then
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
        Helpers.renderWarnings state.Warnings
        table [ classes [ Bootstrap.table; Bootstrap.tableHover; Bootstrap.tableStriped ] ] [
            thead [] [
                tr [] [
                    th [] [ str "Naam" ]
                    th [] [ str "Bestand" ]
                    th [] [ str "Leverancier / Maatschappij" ]
                    th [] [ str "Makelaar" ]
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
                        mandatoryContractTypesFor answer
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
                        | ContractType.Other _
                        | ContractType.Insurance _ -> Some (rowsForContract false c)
                        | ContractType.Predefined _ -> None)
            ]
        ]
        div [ classes [ Bootstrap.card; Bootstrap.bgLight; Bootstrap.dInlineBlock ] ] [
            div [ Class Bootstrap.cardBody ] [
                button [ classes [ Bootstrap.btn; Bootstrap.btnPrimary ]; Type "button"; OnClick (fun _ -> CreateNewContract |> dispatch) ] [
                    i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                    str " "
                    str "Contract aanmaken"
                ]
                button [ classes [ Bootstrap.btn; Bootstrap.btnPrimary; Bootstrap.ml2 ]; Type "button"; OnClick (fun _ -> CreateNewInsuranceContract |> dispatch) ] [
                    i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                    str " "
                    str "Verzekeringscontract aanmaken"
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

        if state.QuestionModalIsOpen then renderQuestionsModalComponent state dispatch
    ]
    |> withPageHeader "Contracten"

let render =
    FunctionComponent.Of (
        (fun (props: ContractsPageProps) -> React.elmishComponent ("ContractsPage", init props, update, view))
        , "ContractsPageComponent"
        , equalsButFunctions
    )