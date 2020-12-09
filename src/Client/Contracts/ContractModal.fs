module Client.Contracts.ContractModal

open System
open Elmish
open Elmish.React
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Read
open Shared.Remoting
open Shared.Library

open Client.Components
open Client.Components.BasicModal
open Client.Components.SelectionList
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Shared.MediaLibrary
open Client.Upload
open Client.Contracts.Translations

type Model = {
    IsOpen: bool
    Contract: (Contract * bool) option
    BuildingId: Guid
    State: State
    AllOrganizations: OrganizationListItem list
    OnOk: (Contract * bool) -> unit
    OnCanceled: unit -> unit
}
and State =
    | EditingContract
    | SelectingSupplier
    | SelectingBroker of InsuranceContract
    | RemotingError of exn

type Message =
    | LoadOrganizations
    | OrganizationsLoaded of OrganizationListItem list

    | SearchSupplier
    | CancelSelectSupplier
    | SelectSupplier of OrganizationListItem

    | SearchBroker of InsuranceContract
    | CancelSelectBroker
    | SelectBroker of InsuranceContract * OrganizationListItem

    | ContractFileUploaded of MediaFile
    | ContractFileRemoved of fileId: Guid
    | ChangeContractName of string
    | CloseModal
    | SaveModal
    | RemotingErrorOccured of exn

type ContractModalProps = {|
    IsOpenOn: (Contract * bool) option
    BuildingId: Guid
    OnOk: (Contract * bool) -> unit
    OnCanceled: unit -> unit
|}

let init (props: ContractModalProps) =
    {
        IsOpen = props.IsOpenOn.IsSome
        BuildingId = props.BuildingId
        Contract = match props.IsOpenOn with | Some (contract) -> Some contract | None -> None
        State = EditingContract
        AllOrganizations = []
        OnOk = props.OnOk
        OnCanceled = props.OnCanceled
    }, Cmd.ofMsg LoadOrganizations

let update (msg: Message) (model: Model) =
    let withContractDo (f: Contract -> Contract) =
        match model.Contract with
        | Some (contract, isNew) -> { model with Contract = Some (f contract, isNew) }
        | None -> model

    match msg with
    | LoadOrganizations ->
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).GetOrganizations model.BuildingId
                OrganizationsLoaded
                RemotingErrorOccured
        model, cmd
    | OrganizationsLoaded orgs ->
        { model with AllOrganizations = orgs }, Cmd.none
    | SearchSupplier ->
        { model with State = State.SelectingSupplier }, Cmd.none
    | CancelSelectSupplier ->
        { model with State = State.EditingContract }, Cmd.none
    | SelectSupplier organization ->
        let newState = withContractDo (fun c -> { c with ContractOrganization = Some organization })
        { newState with State = State.EditingContract }, Cmd.none
    | SearchBroker insuranceContract ->
        { model with State = State.SelectingBroker insuranceContract }, Cmd.none
    | CancelSelectBroker ->
        { model with State = State.EditingContract }, Cmd.none
    | SelectBroker (insuranceContract, organization) ->
        let newState = withContractDo (fun c -> { c with ContractType = InsuranceContractType { insuranceContract with Broker = Some organization } })
        { newState with State = State.EditingContract }, Cmd.none
    | ContractFileUploaded mediaFile ->
        withContractDo (fun c -> { c with ContractFiles = mediaFile::c.ContractFiles }), Cmd.none
    | ContractFileRemoved mediaFileId ->
        withContractDo (fun c -> { c with ContractFiles = c.ContractFiles |> List.filter (fun contractFile -> contractFile.FileId <> mediaFileId) }), Cmd.none
    | ChangeContractName newName ->
        withContractDo (fun c -> 
            match c.ContractType with
            | OtherContractType _oldName -> { c with ContractType = OtherContractType newName }
            | InsuranceContractType contract -> { c with ContractType = InsuranceContractType { contract with Name = newName } }
            | PredefinedContractType _ -> c
        ), Cmd.none
    | CloseModal ->
        do model.OnCanceled ()
        model, Cmd.none
    | SaveModal ->
        match model.Contract with
        | Some contract ->
            do model.OnOk (contract)
        | None ->
            do ()
        model, Cmd.none
    | RemotingErrorOccured exn ->
        { model with State = RemotingError exn }, Cmd.none

let renderContractEditView (contract: Contract) dispatch =
    div [] [
        yield! [
            match contract.ContractType with
            | ContractType.PredefinedContractType predefined ->
                formGroup [
                    Label "Naam"
                    Input [
                        HTMLAttr.Disabled true
                        Value (translatePredefinedType predefined)
                    ]
                ]
            | ContractType.OtherContractType name ->
                formGroup [
                    Label "Naam"
                    Input [
                        Helpers.valueOrDefault name
                        OnChange (fun e -> ChangeContractName e.Value |> dispatch)
                    ]
                ]
            | ContractType.InsuranceContractType insuranceContract ->
                [
                    formGroup [
                        Label "Naam"
                        Input [
                            Helpers.valueOrDefault insuranceContract.Name
                            OnChange (fun e -> ChangeContractName e.Value |> dispatch)
                        ]
                    ]
                    let brokerName = insuranceContract.Broker |> Option.map (fun broker -> broker.Name) |> Option.defaultValue ""
                    formGroup [
                        Label "Verzekeringsmakelaar"
                        Input [
                            Type "text"
                            Style [ Cursor "pointer"; BackgroundColor "unset" ]
                            ReadOnly true
                            OnClick (fun _ -> dispatch (SearchBroker insuranceContract))
                            Helpers.valueOrDefault brokerName
                        ]
                        InputAppend [
                            button [ 
                                classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ] 
                                OnClick (fun _ -> dispatch (SearchBroker insuranceContract))
                            ] [
                                span [ classes [ FontAwesome.fas; FontAwesome.faSearch ] ] []
                            ]
                        ]
                    ]
                ]
                |> fragment []

            let supplierLabel =
                match contract.ContractType with
                | ContractType.InsuranceContractType _ -> "Verzekeringsmaatschappij"
                | _ -> "Leverancier"

            let orgName = contract.ContractOrganization |> Option.map (fun org -> org.Name)

            formGroup [
                Label supplierLabel
                Input [
                    Type "text"
                    Style [ Cursor "pointer"; BackgroundColor "unset" ]
                    ReadOnly true
                    OnClick (fun _ -> dispatch SearchSupplier)
                    Helpers.valueOrDefault orgName
                ]
                InputAppend [
                    button [ 
                        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ] 
                        OnClick (fun _ -> dispatch SearchSupplier)
                    ] [
                        span [ classes [ FontAwesome.fas; FontAwesome.faSearch ] ] []
                    ]
                ]
            ]

            filePond
                {|
                    BuildingId = Some contract.BuildingId
                    Partition = Partitions.Contracts
                    EntityId = contract.ContractId
                    Options = [
                        AllowMultiple true
                        MaxFiles 10
                        InitialFiles (contract.ContractFiles |> List.map (fun file -> file.FileId))
                        OnProcessFile (fun error filepondfile ->
                            if String.IsNullOrWhiteSpace(error) then
                                filepondfile
                                |> FilePondFile.toMediaFile Partitions.Contracts (Some contract.BuildingId) contract.ContractId
                                |> ContractFileUploaded
                                |> dispatch)
                        OnRemoveFile (fun error filepondfile ->
                            if String.IsNullOrWhiteSpace(error) then
                                Guid.Parse(filepondfile.serverId)
                                |> ContractFileRemoved
                                |> dispatch)
                    ]
                |}
        ]
    ]

let renderOrganizationSelectionList (list: OrganizationListItem list) (selected: OrganizationListItem option) (onSelected: OrganizationListItem -> unit) =
    SelectionList.render (
        {|
            SelectionMode = SelectionMode.SingleSelect
            AllItems = list |> List.sortBy (fun o -> o.Name)
            SelectedItems = match selected with | Some s -> [ s ] | None -> []
            OnSelectionChanged = List.head >> onSelected
            ListItemToString = (fun org -> org.Name)
        |}, "OrganizationSelectionList")

let modalContent model dispatch =
    match model.State, model.Contract with
    | EditingContract, Some (contract, _isNew) ->
        renderContractEditView contract dispatch
    | SelectingSupplier, Some (contract, _isNew) ->
        renderOrganizationSelectionList 
            model.AllOrganizations 
            contract.ContractOrganization
            (fun org -> SelectSupplier org |> dispatch)
    | SelectingBroker insuranceContract, _ ->
        renderOrganizationSelectionList
            model.AllOrganizations
            insuranceContract.Broker
            (fun org -> SelectBroker (insuranceContract, org) |> dispatch)
    | EditingContract, _
    | SelectingSupplier, _
    | State.RemotingError _, _ ->
        div [] [ str "Er is iets misgelopen bij het ophalen van de gegevens. Gelieve de pagine te verversen en opnieuw te proberen." ]

let renderModalButtons model dispatch =
    let saveButton =
        button [ 
            classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
            OnClick (fun _ -> dispatch SaveModal) 
        ] [
            i [ classes [ FontAwesome.fa; FontAwesome.faSave ] ] []
            str " "
            str "Bewaren"
        ]

    let cancelButton (onClick: unit -> unit)=
        button [ 
            classes [ Bootstrap.btn; Bootstrap.btnOutlineDanger ]
            OnClick (fun _ -> onClick ()) 
        ] [ 
            str "Annuleren" 
        ]

    match model.State with
    | EditingContract ->
        [ cancelButton (fun () -> dispatch CloseModal); saveButton ]
    | SelectingSupplier ->
        [ cancelButton (fun () -> dispatch CancelSelectSupplier) ]
    | SelectingBroker _ ->
        [ cancelButton (fun () -> dispatch CancelSelectBroker) ]
    | _ ->
        [ cancelButton (fun () -> dispatch CloseModal) ]


let view (model: Model) dispatch =
    BasicModal.render 
        {| 
            ModalProps = [
                IsOpen model.IsOpen
                DisableBackgroundClick true
                OnDismiss (fun _ -> dispatch CloseModal)
                Header [
                    HeaderProp.HasDismissButton true
                ]
                Body [
                    modalContent model dispatch
                ]
                Footer [
                    yield FooterProp.Buttons (renderModalButtons model dispatch)
                    //yield FooterProp.ShowDismissButton (Some "Annuleren")
                ]
            ]           
        |}

let render (props: ContractModalProps) =
    React.elmishComponent ("ContractModal", init props, update, view)