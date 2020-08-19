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
}
and State =
    | EditingContract
    | SelectingOrganization
    | RemotingError of exn

type Message =
    | DeleteContractFile
    | LoadOrganizations
    | OrganizationsLoaded of OrganizationListItem list
    | SearchOrganization
    | CancelSelectOrganization
    | ContractFileUploaded of MediaFile
    | SelectOrganization of OrganizationListItem
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
    }, Cmd.ofMsg LoadOrganizations

let update (onOk: Contract * bool -> unit) (onCanceled: unit -> unit) (msg: Message) (model: Model) =
    let withContractDo (f: Contract -> Contract) =
        match model.Contract with
        | Some (contract, isNew) -> { model with Contract = Some (f contract, isNew) }
        | None -> model

    match msg with
    | DeleteContractFile ->
        withContractDo (fun c -> { c with ContractFile = None }), Cmd.none
    | LoadOrganizations ->
        let cmd =
            Cmd.OfAsync.either
                (Client.Remoting.getRemotingApi()).GetOrganizations {| BuildingId = model.BuildingId |}
                OrganizationsLoaded
                RemotingErrorOccured
        model, cmd
    | OrganizationsLoaded orgs ->
        { model with AllOrganizations = orgs }, Cmd.none
    | SearchOrganization ->
        { model with State = State.SelectingOrganization }, Cmd.none
    | CancelSelectOrganization ->
        { model with State = State.EditingContract }, Cmd.none
    | ContractFileUploaded mediaFile ->
        withContractDo (fun c -> { c with ContractFile = Some mediaFile }), Cmd.none
    | SelectOrganization organization ->
        withContractDo (fun c -> { c with ContractOrganization = Some organization }), Cmd.none
    | ChangeContractName newName ->
        withContractDo (fun c -> 
            match c.ContractType with
            | OtherContractType _oldName -> { c with ContractType = OtherContractType newName }
            | PredefinedContractType _ -> c
        ), Cmd.none
    | CloseModal ->
        do onCanceled ()
        model, Cmd.none
    | SaveModal ->
        match model.Contract with
        | Some contract ->
            do onOk (contract)
        | None ->
            do ()
        model, Cmd.none
    | RemotingErrorOccured exn ->
        { model with State = RemotingError exn }, Cmd.none

let renderContractEditView (contract: Contract) dispatch =
    let orgName = contract.ContractOrganization |> Option.map (fun org -> org.Name)

    div [] [
        yield! [
            match contract.ContractType with
            | ContractContractType.PredefinedContractType predefined -> 
                div [ Class Bootstrap.formGroup ] [
                    label [] [ str "Naam" ]
                    div [] [
                        input [ Class Bootstrap.formControl; HTMLAttr.Disabled true; Value (translatePredefinedType predefined) ]
                    ]
                ]
            | ContractContractType.OtherContractType name ->
                div [ Class Bootstrap.formGroup ] [
                    label [] [ str "Naam" ]
                    div [] [
                        input [ Class Bootstrap.formControl; Helpers.valueOrDefault name; OnChange (fun e -> ChangeContractName e.Value |> dispatch) ]
                    ]
                ]

            div [ Class Bootstrap.formGroup ] [
                label [] [ str "Organisatie" ]
                div [ Class Bootstrap.inputGroup; OnClick (fun _ -> dispatch SearchOrganization) ] [
                    input [ classes [ Bootstrap.formControl; "pointer" ]; Helpers.valueOrDefault orgName; ]
                    div [ Class Bootstrap.inputGroupAppend ] [
                        button [ classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]  ] [
                            span [ classes [ FontAwesome.fas; FontAwesome.faLink ] ] []
                        ]
                    ]
                ]
            ]

            match contract.ContractFile with
            | None ->
                filePond 
                    {| 
                        Partition = Partitions.Contracts; 
                        EntityId = contract.ContractId
                        Options = [
                            AllowMultiple false
                            MaxFiles 1
                            OnProcessFile (fun error filepondfile ->
                                if String.IsNullOrWhiteSpace(error) then
                                    filepondfile
                                    |> FilePondFile.toMediaFile Partitions.Contracts contract.ContractId
                                    |> ContractFileUploaded
                                    |> dispatch)
                        ]
                    |}
            | Some mediaFile ->
                div [ Class Bootstrap.formGroup ] [
                    label [] [ str "Bestand" ]
                    div [ Class Bootstrap.inputGroup ] [
                        a [ Href (downloadUri Partitions.Contracts mediaFile.FileId); Target "_blank"; Class Bootstrap.formControl ] [
                            str (sprintf "%s (%s)" mediaFile.FileName (mediaFile.FileSizeString ()))
                        ]
                        div [ Class Bootstrap.inputGroupAppend ] [
                            button [ classes [ Bootstrap.btn; Bootstrap.btnOutlineDanger ]; Type "button"; OnClick (fun _ -> dispatch DeleteContractFile) ] [
                                span [ classes [ FontAwesome.fas; FontAwesome.faTrashAlt ] ] []
                            ]
                        ]
                    ]
                ]
        ]
    ]

let renderOrganizationSelectionList (list: OrganizationListItem list) (selected: OrganizationListItem option) dispatch =
    SelectionList.render (
        {|
            SelectionMode = SelectionMode.SingleSelect
            AllItems = list |> List.sortBy (fun o -> o.Name)
            SelectedItems = match selected with | Some s -> [ s ] | None -> []
            OnSelectionChanged = fun selection -> SelectOrganization (selection |> List.head) |> dispatch
            DisplayListItem = (fun org -> str org.Name)
        |}, "OrganizationSelectionList")

let modalContent model dispatch =
    match model.State, model.Contract with
    | EditingContract, Some (contract, _isNew) ->
        renderContractEditView contract dispatch
    | SelectingOrganization, Some (contract, _isNew) ->
        renderOrganizationSelectionList 
            model.AllOrganizations 
            contract.ContractOrganization
            dispatch
    | EditingContract, _
    | SelectingOrganization, _
    | State.RemotingError _, _ ->
        div [] [ str "Er is iets misgelopen bij het ophalen van de gegevens. Gelieve de pagine te verversen en opnieuw te proberen." ]

let renderModalButtons model dispatch =
    let closeButton =
        button 
            [ classes [ Bootstrap.btn; Bootstrap.btnDanger ]; OnClick (fun _ -> dispatch CloseModal) ]
            [ str "Annuleren" ]

    let saveButton =
        button
            [ classes [ Bootstrap.btn; Bootstrap.btnSuccess ]; OnClick (fun _ -> dispatch SaveModal) ]
            [ str "Bewaren" ]

    let cancelButton =
        button 
            [ classes [ Bootstrap.btn; Bootstrap.btnDanger ]; OnClick (fun _ -> dispatch CancelSelectOrganization) ]
            [ str "Annuleren" ]

    match model.State with
    | EditingContract ->
        [ closeButton; saveButton ]
    | SelectingOrganization ->
        [ cancelButton ]
    | _ ->
        [ closeButton ]


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
    React.elmishComponent ("ContractModal", init props, update props.OnOk props.OnCanceled, view)