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
    | OpenContractModal of Contract * isNew: bool
    | CloseContractModal
    | SaveContract of Contract * isNew: bool
    | ContractSaved of Result<unit, SaveContractError>
    | DebouncerSelfMsg  of Debouncer.SelfMessage<Message>
    | RemotingException of exn

type ContractsPageProps = 
    {|
        CurrentBuildingId: Guid
        CurrentUser: User
    |}

//TODO: get contracts, get answers
let init (props: ContractsPageProps) =
    { 
        CurrentBuildingId = props.CurrentBuildingId
        CurrentUser = props.CurrentUser 
        IsLoading = true
        ContractTypeAnswers = []
        Contracts = []
        ContractModalIsOpenOn = None
        Debouncer = Debouncer.create()
    }, Cmd.none

let update (msg: Message) (state: State): State * Cmd<Message> =
    match msg with
    | ContractsRetrieved contracts ->
        { state with Contracts = contracts }, Cmd.none
    | ContractTypeAnswersRetrieved answers ->
        { state with ContractTypeAnswers = answers |> List.map (fun a -> { IsSaving = false; Payload = a  }) }, Cmd.none
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
            state.Debouncer
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
        state, OpenContractModal (contract, true) |> Cmd.ofMsg
    | CreateMandatoryContract predefinedType ->
        let contract: Contract = {
            ContractId = Guid.NewGuid()
            BuildingId = state.CurrentBuildingId
            ContractType = PredefinedContractType predefinedType
            ContractOrganization = None
            ContractFile = None
        }
        state, OpenContractModal (contract, true) |> Cmd.ofMsg
    | EditContract contract ->
        state, OpenContractModal (contract, false) |> Cmd.ofMsg
    | DeleteContract contract ->
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).DeleteContract (state.CurrentBuildingId, contract.ContractId)
                ContractDeleted
                RemotingException
        state, cmd
    | ContractDeleted (Ok ()) ->
        state, showSuccessToastCmd "Contract verwijderd"
    | ContractDeleted (Error e) ->
        match e with
        | DeleteContractError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om een contract te verwijderen..."
        | DeleteContractError.NotFound ->
            printf "The contract that was being deleted is not found in the DB, somehow? O_o"
            state, Cmd.none
    | OpenContractModal (c, isNew) ->
        { state with ContractModalIsOpenOn = Some (c, isNew) }, Cmd.none
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

        { state with Contracts = newContracts }, cmd
    | ContractSaved (Ok ()) ->
        state, showSuccessToastCmd "Uw contract werd bewaard"
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
        
let partition = "contracts"

let view (state: State) (dispatch: Message -> unit) =
    let renderQuestion (question: ContractTypeQuestion) =
        let answer = state.ContractTypeAnswers |> List.tryFind (fun answer -> answer.Payload.Question = question)

        div [] [
            yield label [] [ str (translateQuestion question) ]

            match answer with
            | Some savable ->
                let positiveClass = if savable.Payload.IsTrue then Bootstrap.btnPrimary else Bootstrap.btnLight
                let negativeClass = if savable.Payload.IsTrue then Bootstrap.btnLight else Bootstrap.btnPrimary

                if savable.IsSaving then
                    yield div [] [
                        button [ Type "button"; classes [ Bootstrap.btn; positiveClass; Bootstrap.mb2 ]; HTMLAttr.Disabled true ] [ str "Ja" ]
                        button [ Type "button"; classes [ Bootstrap.btn; negativeClass; Bootstrap.mb2 ]; HTMLAttr.Disabled true ] [ str "Nee" ]
                    ]
                else
                    yield div [] [
                        button [ Type "button"; classes [ Bootstrap.btn; positiveClass; Bootstrap.mb2 ]; OnClick (fun _ -> SetAnswer (question, true) |> dispatch) ] [ str "Ja" ]
                        button [ Type "button"; classes [ Bootstrap.btn; negativeClass; Bootstrap.mb2 ]; OnClick (fun _ -> SetAnswer (question, false) |> dispatch) ] [ str "Nee" ]                    
                    ]
            | None ->
                yield div [] [
                    button [ Type "button"; classes [ Bootstrap.btn; Bootstrap.btnLight; Bootstrap.mb2 ]; OnClick (fun _ -> SetAnswer (question, true) |> dispatch) ] [ str "Ja" ]
                    button [ Type "button"; classes [ Bootstrap.btn; Bootstrap.btnLight; Bootstrap.mb2 ]; OnClick (fun _ -> SetAnswer (question, false) |> dispatch) ] [ str "Nee" ]                                    
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
                    span [] [
                        a [ Href (downloadUri partition mediaFile.FileId) ] [
                            span [ classes [ FontAwesome.fas; FontAwesome.faCloudDownloadAlt ] ] []
                        ]
                    ]
                | None -> str "Geen bestand" ]
            td [] [ 
                match c.ContractOrganization with 
                | Some org -> 
                    span [] [ str org.Name ]
                    span [ Class Bootstrap.floatRight ] [
                        span [ Title "Bekijken in huidige tab" ] [
                            a [ Class "pointer"; OnClick (fun _ -> ShowOrganizationDetails org.OrganizationId |> dispatch) ] [
                                span [ classes [ FontAwesome.fas; FontAwesome.faEye ] ] []
                            ]
                        ]
                        str " "
                        span [ Title "Openen in nieuwe tab" ] [
                            a [ Target "_blank"; Href (Client.Routing.generateUrl (Page.OrganizationDetails { BuildingId = state.CurrentBuildingId; DetailId = org.OrganizationId })) ] [
                                span [ classes [ FontAwesome.fas; FontAwesome.faExternalLinkAlt ] ] []
                            ]
                        ]
                    ]
                | None ->
                    null
            ]
            td [] [
                a [ Class "pointer"; OnClick (fun _ -> EditContract c |> dispatch) ] [
                    span [ classes [ FontAwesome.fas; FontAwesome.faEdit ] ] []
                ]
            ]
            td [] [
                match c.ContractFile with 
                | None when isMandatory -> 
                    span [ Class Bootstrap.textDanger ] [
                        span [ classes [ FontAwesome.fa; FontAwesome.faExclamationTriangle ] ] []  
                        str "Wettelijk verplicht"
                    ]
                | Some when isMandatory-> 
                    span [ classes [ FontAwesome.fa; FontAwesome.faCheckSquare ] ] []
                | _ -> 
                    a [ classes [ "pointer"; Bootstrap.textDanger ]; OnClick (fun _ -> DeleteContract c |> dispatch) ] [
                        span [ classes [ FontAwesome.fas; FontAwesome.faTrashAlt ] ] []
                    ]
            ]
        ]

    let rowsForPredefinedContractType (predefined: PredefinedContractType)  =
        let contracts = state.Contracts |> List.filter (fun contract -> contract.ContractType = PredefinedContractType predefined)
        match contracts with
        | [] ->
            [
                tr [] [
                    td [] [ str (translatePredefinedType predefined ) ]
                    td [] [ str "Geen bestand" ]
                    td [] []
                    td [ Class Bootstrap.textDanger ] [ 
                        span [ classes [ FontAwesome.fas; FontAwesome.faExclamationTriangle ] ] []
                        str " Wettelijk verplicht"
                    ]
                    td [] [
                        a [ classes [ "pointer"; Bootstrap.textPrimary ]; OnClick (fun _ -> CreateMandatoryContract predefined |> dispatch) ] [
                            span [ classes [ FontAwesome.fas; FontAwesome.faCloudUploadAlt ] ] []
                            str " Uploaden"
                        ]
                    ]
                ]
            ]
        | contracts ->
            contracts |> List.map (rowsForContract true)

    div [ Class Bootstrap.row ] [
        div [ classes [ Bootstrap.colMd4; Bootstrap.col12 ] ] [
            yield
                div [ Class Bootstrap.card ] [
                    div [ Class Bootstrap.cardBody ] [
                        yield!
                            ContractTypeQuestion.AllValues 
                            |> Array.map renderQuestion
                    ]
                ]
        ]
        div [ classes [ Bootstrap.colMd8; Bootstrap.col12 ] ] [
            table [ classes [ Bootstrap.table; Bootstrap.tableHover ] ] [
                thead [] [
                    tr [] [
                        th [] [ str "Naam" ]
                        th [] [ str "Bestand" ]
                        th [] [ str "Organisatie" ]
                        th [] []
                        th [] []
                    ]
                ]
                tbody [] [
                    yield! 
                        MandatoryContractTypes  
                        |> Array.toList
                        |> List.collect rowsForPredefinedContractType

                    yield!
                        state.ContractTypeAnswers 
                        |> List.collect (fun answer ->
                            mandatoryContractTypesFor answer.Payload
                            |> Array.toList
                            |> List.collect rowsForPredefinedContractType
                        )

                    yield!
                        state.Contracts
                        |> List.choose (fun c -> 
                            match c.ContractType with 
                            | ContractContractType.OtherContractType _ -> Some (rowsForContract false c) 
                            | _ -> None)

                    yield
                        tr [] [
                            td [ ColSpan 4 ] []
                            td [] [
                                button [ classes [ Bootstrap.btn; Bootstrap.btnPrimary ]; Type "button"; OnClick (fun _ -> CreateNewContract |> dispatch) ] [
                                    str "Contract aanmaken"
                                ]
                            ]
                        ]
                ]
            ]
        ]

        ContractModal.render
            {|
                IsOpenOn = state.ContractModalIsOpenOn
                BuildingId = state.CurrentBuildingId
                OnOk = (fun (contract, isNew) -> SaveContract (contract, isNew) |> dispatch)
                OnCanceled = (fun _ -> Message.CloseContractModal |> dispatch)
            |}
    ]

let render (props: ContractsPageProps) =
    React.elmishComponent ("ContractsPage", init props, update, view)