module Client.Organizations.OrganizationEditComponent

open System
open Elmish
open Elmish.React
open Elmish.SweetAlert
open Fable.React
open Fable.React.Props

open Shared.Read
open Shared.Write
open Shared.Library
open Shared.ConstrainedTypes
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Library
open Client.Components
open Client.Components.BasicModal
open Feliz

type State = {
    Organization: Organization
    CreateOrUpdate: CreateOrUpdate
    Errors: (string * string) list
    AllOrganizationTypes: OrganizationType list
    ContactPersonModalState: ContactPersonModalState
    OrganizationTypeModalState: OrganizationTypeModalState
    VatNumberCheckingState: VatNumberCheckingState option
    BankAccountEditComponentState: BankAccountEditComponent.State
}
and ContactPersonModalState =
    | Closed
    | Creating of ContactPersonEditComponent.State
    | Editing of ContactPersonEditComponent.State
and OrganizationTypeModalState =
    | Closed
    | Opened of OrganizationType list
and VatNumberCheckingState =
    | CheckingVatNumber
    | VatNumberCheckingFailed

type Message =
    | NameChanged of string
    | AddressChanged of Address
    | UsesVatNumberChanged of bool

    | AddContactPerson
    | EditContactPerson of ContactPerson
    | EditContactPersonCanceled
    | ContactPersonCreated of ContactPerson
    | ContactPersonEdited of ContactPerson
    | ContactPersonRemoved of Guid
    | ContactPersonEditComponentMsg of ContactPersonEditComponent.Message

    | OrganizationNumberChanged of string
    | MainTelephoneNumberChanged of string
    | MainTelephoneNumberCommentChanged of string
    | MainEmailAddressChanged of string
    | MainEmailAddressCommentChanged of string
    | OtherContactMethodAdded
    | OtherContactMethodRemoved of index: int
    | OtherContactMethodDescriptionChanged of index: int * newDescription: string
    | OtherContactMethodTypeChanged of index: int * newType: ContactMethodType
    | OtherContactMethodValueChanged of index: int * newValue: string

    | AllOrganizationTypesLoaded of OrganizationType list
    | RemoveOrganizationType of organizationType: OrganizationType
    | AddOrganizationType
    | SelectOrganizationType of organizationType: OrganizationType
    | ChangeOrganizationTypesCanceled
    | OrganizationTypesChanged of organizationTypes: OrganizationType list

    | BankAccountAdded
    | BankAccountChanged of (int * BankAccount)
    | BankAccountRemoved of int

    | VatNumberChanged of string
    | VerifyVatNumber
    | VatNumberVerified of result: Result<VatNumberValidationResponse, string>

    | RemotingError of exn
    | BankAccountEditComponentMsg of BankAccountEditComponent.Msg

type OrganizationEditComponentProps = 
    {|
        Organization: Organization option
        Building: BuildingListItem option
    |}

let init (props: OrganizationEditComponentProps) =
    let organization, createOrUpdate =
        match props.Organization with
        | Some organization -> organization, Update
        | None        -> Organization.Init (props.Building |> Option.map (fun b -> b.BuildingId), true), Create

    let cmd =
        Cmd.OfAsync.either
            (Client.Remoting.getRemotingApi()).GetOrganizationTypes ()
            AllOrganizationTypesLoaded
            RemotingError

    let componentState, componentCmd =
        BankAccountEditComponent.init
            {|
                BankAccounts = organization.BankAccounts
                BasePath = nameof (organization.BankAccounts)
            |}

    { 
        Organization = organization
        CreateOrUpdate = createOrUpdate
        Errors = [] 
        ContactPersonModalState = ContactPersonModalState.Closed
        OrganizationTypeModalState = Closed
        AllOrganizationTypes = []
        VatNumberCheckingState = None
        BankAccountEditComponentState = componentState
    }, Cmd.batch [ 
        cmd
        componentCmd |> Cmd.map BankAccountEditComponentMsg
    ]

let update (message: Message) (state: State): State * Cmd<Message> =
    let changeOrganization alterFunc =
        { state with Organization = alterFunc state.Organization }

    let changeOtherContactMethods otherContactMethods =
        changeOrganization (fun o -> { o with OtherContactMethods = otherContactMethods })

    let recalculateValidationErrors (state: State) =
        match state.Errors with
        | [] -> state
        | _errors ->
            match ValidatedOrganization.Validate state.Organization with
            | Ok _validated -> state
            | Error validationErrors -> { state with Errors = validationErrors }

    match message with
    | NameChanged x ->
        changeOrganization (fun o -> { o with Name = x |> String.toOption |> Option.defaultValue "" }), Cmd.none
    | UsesVatNumberChanged x ->
        changeOrganization (fun o -> { o with UsesVatNumber = x }), Cmd.none
    | OrganizationNumberChanged x ->
        changeOrganization (fun o -> { o with OrganizationNumber = if String.IsNullOrEmpty x then None else Some x }), Cmd.none
    | MainTelephoneNumberChanged x ->
        changeOrganization (fun o -> { o with MainTelephoneNumber = x |> String.toOption }), Cmd.none
    | MainTelephoneNumberCommentChanged x ->
        changeOrganization (fun o -> { o with MainTelephoneNumberComment = x |> String.toOption }), Cmd.none
    | MainEmailAddressChanged x ->
        changeOrganization (fun o -> { o with MainEmailAddress = x |> String.toOption }), Cmd.none
    | MainEmailAddressCommentChanged x ->
        changeOrganization (fun o -> { o with MainEmailAddressComment = x |> String.toOption }), Cmd.none
    | OtherContactMethodAdded ->
        let newOtherContactMethods = (ContactMethod.Init())::state.Organization.OtherContactMethods
        changeOtherContactMethods newOtherContactMethods, Cmd.none
    | OtherContactMethodRemoved cmIndex ->
        let newOtherContactMethods = 
            state.Organization.OtherContactMethods 
            |> List.indexed
            |> List.choose (fun (index, cm) -> if index = cmIndex then None else Some cm)
        changeOtherContactMethods newOtherContactMethods, Cmd.none
    | OtherContactMethodDescriptionChanged (cmIndex, newDescription) ->
        let newOtherContactMethods = state.Organization.OtherContactMethods |> List.mapi (fun index cm ->
            if index = cmIndex then { cm with Description = newDescription } else cm
        )
        changeOtherContactMethods newOtherContactMethods, Cmd.none
    | OtherContactMethodTypeChanged (cmIndex, newType) ->
        let newOtherContactMethods = state.Organization.OtherContactMethods |> List.mapi (fun index cm ->
            if index = cmIndex then { cm with ContactMethodType = newType } else cm
        )
        changeOtherContactMethods newOtherContactMethods, Cmd.none
    | OtherContactMethodValueChanged (cmId, newValue) ->
        let newOtherContactMethods = state.Organization.OtherContactMethods |> List.mapi (fun index cm ->
            if index = cmId then { cm with Value = newValue } else cm
        )
        changeOtherContactMethods newOtherContactMethods, Cmd.none
    | AddressChanged newAddress ->
        changeOrganization (fun org -> { org with Address = newAddress }), Cmd.none
    | AddContactPerson ->
        let componentState, componentCmd = ContactPersonEditComponent.init (ContactPerson.Init state.Organization.OrganizationId state.Organization.BuildingId)
        { state with ContactPersonModalState = Creating componentState }, componentCmd |> Cmd.map ContactPersonEditComponentMsg
    | EditContactPerson cp ->
        let componentState, componentCmd = ContactPersonEditComponent.init cp
        { state with ContactPersonModalState = Editing componentState }, componentCmd |> Cmd.map ContactPersonEditComponentMsg
    | EditContactPersonCanceled ->
        { state with ContactPersonModalState = ContactPersonModalState.Closed }, Cmd.none
    | ContactPersonCreated cp ->
        let newContactPersons = state.Organization.ContactPersons @ [ cp ]
        let newState = changeOrganization (fun org -> { org with ContactPersons = newContactPersons })
        { newState with ContactPersonModalState = ContactPersonModalState.Closed }, Cmd.none
    | ContactPersonEdited cp ->
        let newContactPersons = 
            state.Organization.ContactPersons 
            |> List.map (fun existing -> if cp.Person.PersonId = existing.Person.PersonId then cp else existing)
        let newState = changeOrganization (fun org -> { org with ContactPersons = newContactPersons })
        { newState with ContactPersonModalState = ContactPersonModalState.Closed }, Cmd.none
    | ContactPersonRemoved personId ->
        let newContactPersons = 
            state.Organization.ContactPersons 
            |> List.filter (fun existing -> existing.Person.PersonId <> personId)
        changeOrganization (fun org ->  { org with ContactPersons = newContactPersons }), Cmd.none
    | ContactPersonEditComponentMsg msg ->
        match state.ContactPersonModalState with
        | ContactPersonModalState.Closed -> state, Cmd.none
        | Editing s ->
            let componentState, componentCmd = ContactPersonEditComponent.update msg s
            { state with ContactPersonModalState = Editing componentState }, componentCmd |> Cmd.map ContactPersonEditComponentMsg
        | Creating s ->
            let componentState, componentCmd = ContactPersonEditComponent.update msg s
            { state with ContactPersonModalState = Creating componentState }, componentCmd |> Cmd.map ContactPersonEditComponentMsg
    | AllOrganizationTypesLoaded allOrganizationTypes ->
        { state with AllOrganizationTypes = allOrganizationTypes }, Cmd.none
    | RemoveOrganizationType organizationType ->
        let newOrgTypes = state.Organization.OrganizationTypes |> List.filter (fun ot -> ot <> organizationType)
        changeOrganization (fun org -> { org with OrganizationTypes = newOrgTypes }), Cmd.none
    | AddOrganizationType ->
        { state with OrganizationTypeModalState = Opened state.Organization.OrganizationTypes }, Cmd.none
    | SelectOrganizationType organizationType ->
        match state.OrganizationTypeModalState with
        | Opened types ->
            if types |> List.contains organizationType 
            then
                { state with OrganizationTypeModalState = Opened (types |> List.filter (fun ot -> ot <> organizationType)) }, Cmd.none
            else
                { state with OrganizationTypeModalState = Opened (types @ [ organizationType ]) }, Cmd.none
        | Closed ->
            state, Cmd.none
    | ChangeOrganizationTypesCanceled ->
        { state with OrganizationTypeModalState = Closed }, Cmd.none
    | OrganizationTypesChanged organizationTypes ->
        let newState = changeOrganization (fun org -> { org with OrganizationTypes = organizationTypes })
        { newState with OrganizationTypeModalState = Closed }, Cmd.none
    | VatNumberChanged vatNumber ->
        let newState = changeOrganization (fun org -> { org with VatNumber = (if String.IsNullOrEmpty vatNumber then None else Some vatNumber); VatNumberVerifiedOn = None })
        { newState with VatNumberCheckingState = None }, Cmd.none
    | BankAccountAdded ->
        changeOrganization (fun org -> { org with BankAccounts = org.BankAccounts @ [ BankAccount.Init () ] })
        , Cmd.none
    | BankAccountChanged (index, updatedBankAccount) ->
        changeOrganization (fun org ->
            let updatedBankAccounts =
                org.BankAccounts
                |> List.mapi (fun i bankAccount -> if i = index then updatedBankAccount else bankAccount)
            { org with BankAccounts = updatedBankAccounts }
        ), Cmd.none
    | BankAccountRemoved index ->
        changeOrganization (fun org ->
            let updatedBankAccounts =
                org.BankAccounts
                |> List.mapi (fun i bankAccount -> if i = index then None else Some bankAccount)
                |> List.choose id
            { org with BankAccounts = updatedBankAccounts }
        ), Cmd.none
    | VerifyVatNumber ->
        match VatNumber.TryParse (defaultArg state.Organization.VatNumber "") with
        | Ok vatNumber ->
            let cmd =
                Cmd.OfAsync.either
                    (Client.Remoting.getRemotingApi()).VerifyVatNumber vatNumber
                    VatNumberVerified
                    RemotingError
            { state with VatNumberCheckingState = Some CheckingVatNumber }, cmd
        | Error e ->
            { state with VatNumberCheckingState = Some VatNumberCheckingFailed }
            , showErrorToastCmd e
    | VatNumberVerified r ->
        match r with
        | Ok verification ->
            if verification.IsValid then
                let message = 
                    sprintf "De volgende gegevens zijn geassocieerd met het opgegeven BTW nummer: %s, %s" 
                        (defaultArg verification.Name "") 
                        (verification.Address |> Option.either string "")
                let alert = SimpleAlert(message).Type(AlertType.Success)
                let newState = changeOrganization (fun org -> { 
                    org with 
                        Name = defaultArg verification.Name org.Name
                        VatNumberVerifiedOn = Some (verification.RequestDate.Date)
                        Address = defaultArg verification.Address org.Address
                })
                { newState with VatNumberCheckingState = None }, SweetAlert.Run(alert)
            else
                { state with VatNumberCheckingState = Some VatNumberCheckingFailed }
                , showErrorToastCmd "Het opgegeven BTW nummer is niet geldig"
        | Error e ->
            { state with VatNumberCheckingState = Some VatNumberCheckingFailed }
            , showErrorToastCmd e
    | RemotingError e ->
        state, showGenericErrorModalCmd e
    | BankAccountEditComponentMsg msg ->
        let componentState, componentCmd =
            BankAccountEditComponent.update msg state.BankAccountEditComponentState
        { state with
            BankAccountEditComponentState = componentState
            Organization = { state.Organization with BankAccounts = componentState.BankAccounts }
        }, componentCmd |> Cmd.map BankAccountEditComponentMsg

    |> (fun (state, cmd) -> state |> recalculateValidationErrors, cmd)

let private contactMethodTypeOptions (currentlySelected: ContactMethodType): FormSelectOption list =
    [
        {
            Key = string PhoneNumber
            Label = "Telnr."
            IsSelected = currentlySelected = PhoneNumber
        }
        {
            Key = string EmailAddress
            Label = "E-mail"
            IsSelected = currentlySelected = EmailAddress
        }
        {
            Key = string WebSite
            Label = "Website"
            IsSelected = currentlySelected = WebSite
        }
        {
            Key = string ContactMethodType.Other
            Label = "Other"
            IsSelected = currentlySelected = ContactMethodType.Other
        }
    ]

let private renderOtherContactMethods (otherContactMethods: ContactMethod list) dispatch =
    [
        yield! otherContactMethods |> List.mapi (fun index c ->
            div [ classes [ Bootstrap.row; "full-width" ] ] [
                div [ Class Bootstrap.colMd ] [
                    formGroup [
                        Label "Type"
                        Select {
                            Identifier = string index
                            OnChanged = (fun newTypeString ->
                                let newType =
                                    match newTypeString with
                                    | s when s = string EmailAddress -> EmailAddress
                                    | s when s = string PhoneNumber  -> PhoneNumber
                                    | s when s = string WebSite      -> WebSite
                                    | _                              -> ContactMethodType.Other
                                OtherContactMethodTypeChanged (index, newType) |> dispatch
                            )
                            Options = contactMethodTypeOptions c.ContactMethodType
                        }
                    ]
                ]
                div [ Class Bootstrap.colMd ] [
                    formGroup [
                        Label (string c.ContactMethodType)
                        Input [
                            Type "text"
                            Helpers.valueOrDefault c.Value
                            OnChange (fun e -> OtherContactMethodValueChanged (index, e.Value) |> dispatch) 
                        ]
                    ]
                ]
                div [ Class Bootstrap.colMd ] [
                    formGroup [ 
                        Label "Omschrijving"
                        Input [ 
                            Type "text"
                            MaxLength 255.0
                            Helpers.valueOrDefault c.Description
                            OnChange (fun e -> OtherContactMethodDescriptionChanged (index, e.Value) |> dispatch) 
                        ]
                    ]
                ]
                div [ Class Bootstrap.colMd ] [
                    div [] [
                        label [ Style [ Visibility "hidden" ]; classes [ Bootstrap.dNone; Bootstrap.dMdBlock ] ] [ str "_" ]
                        div [] [
                            button [ 
                                classes [ Bootstrap.btn; Bootstrap.btnDanger ]
                                OnClick (fun _ -> OtherContactMethodRemoved index |> dispatch) 
                            ] [
                                str "Verwijderen"
                            ] 
                        ]
                    ]
                ]
            ]
        )
        yield
            formGroup [
                OtherChildren [
                    button [ classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]; OnClick (fun _ -> OtherContactMethodAdded |> dispatch) ] [
                        i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                        str " "
                        str "Ander contactmiddel toevoegen"
                    ]
                ]
            ]
    ]

let renderContactPersons (contactPersons: ContactPerson list) dispatch =
    [
        yield! contactPersons |> List.mapi (fun index cp ->
            div [] [
                str (sprintf "%s (%s)" (cp.Person.FullName ()) cp.RoleWithinOrganization)
            ]
        )
        yield
            formGroup [
                OtherChildren [
                    button [ classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]; OnClick (fun _ -> AddContactPerson |> dispatch) ] [
                        i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                        str " "
                        str "Contactpersoon toevoegen"
                    ]
                ]
            ]
    ]

let basicRenderContactPersonModal (contactPersonEditComponentState: ContactPersonEditComponent.State) (dispatch) (onSuccessButton) =
    BasicModal.render
        {|
            ModalProps = [
                IsOpen true
                OnDismiss (fun _ -> EditContactPersonCanceled |> dispatch)
                Header [
                    Title (sprintf "%s" (contactPersonEditComponentState.ContactPerson.Person.FullName ()))
                    HasDismissButton true
                ]
                Body [
                    ContactPersonEditComponent.view contactPersonEditComponentState (ContactPersonEditComponentMsg >> dispatch)
                ]
                Footer [
                    Buttons [ onSuccessButton ]
                    ShowDismissButton (Some "Annuleren")
                ]
            ]
        |}
    

let renderCreateContactPersonModal (state: ContactPersonEditComponent.State) dispatch =
    let successButton = 
        button [ 
            classes [ Bootstrap.btn; Bootstrap.btnPrimary ] 
            OnClick (fun _ -> ContactPersonCreated state.ContactPerson |> dispatch)
        ] [
            i [ classes [ FontAwesome.fa; FontAwesome.faSave ] ] []
            str " "
            str "Contactpersoon aanmaken"
        ]

    basicRenderContactPersonModal state dispatch successButton

let renderEditContactPersonModal (state: ContactPersonEditComponent.State) dispatch =
    let successButton = 
        button [ 
            classes [ Bootstrap.btn; Bootstrap.btnPrimary ] 
            OnClick (fun _ -> ContactPersonEdited state.ContactPerson |> dispatch)
        ] [
            i [ classes [ FontAwesome.fa; FontAwesome.faSave ] ] []
            str " "
            str "Bewaren" 
        ]

    basicRenderContactPersonModal state dispatch successButton

let renderOrganizationType (dispatch: Message -> unit) (organizationType: OrganizationType) =
    span [ classes [ Bootstrap.badge; Bootstrap.badgeSecondary; Bootstrap.mr2 ] ] [ 
        span [ 
            classes [ Bootstrap.badge; Bootstrap.badgeDark; "pointer" ]
            OnClick (fun e -> e.preventDefault(); e.stopPropagation(); RemoveOrganizationType organizationType |> dispatch) 
        ] [
            i [ classes [ FontAwesome.fa; FontAwesome.faTimes ] ] []
        ]
        str (sprintf "  %s" organizationType.Name )
    ]

let renderSelectOrganizationTypes (selectedTypes: OrganizationType list) (allTypes: OrganizationType list) (dispatch: Message -> unit) =
    let successButton = 
        button [ 
            classes [ Bootstrap.btn; Bootstrap.btnPrimary ] 
            OnClick (fun _ -> OrganizationTypesChanged selectedTypes |> dispatch)
        ] [ str "Ok" ]

    BasicModal.render 
        {|
            ModalProps = [
                IsOpen true
                OnDismiss (fun _ -> OrganizationTypesChanged selectedTypes |> dispatch)
                Header [
                    Title "Leveranciertypes toevoegen"
                    HasDismissButton true
                ]
                Body [
                    ul [ Class Bootstrap.listGroup ] [
                        yield! 
                            allTypes 
                            |> List.map (fun organizationType -> 
                                let selected = if selectedTypes |> List.contains organizationType then "active" else ""
                                li 
                                    [ 
                                        classes [ Bootstrap.listGroupItem; selected; "pointer" ]
                                        OnClick (fun _ -> SelectOrganizationType organizationType |> dispatch) 
                                    ] [ 
                                        str organizationType.Name 
                                    ]
                            )
                    ]
                ]
                Footer [
                    Buttons [ successButton ]
                    ShowDismissButton (Some "Annuleren")
                ]
            ]
        |}

let private renderOrganizationNumber state dispatch =
    [
        formGroup [
            Label "BTW-plichtig?"
            Radio {
                Inline = true
                RadioButtons =
                    FormRadioButton.YesNo
                        (state.Organization.UsesVatNumber)
                        (fun useVatNumber -> if state.Organization.UsesVatNumber = useVatNumber then () else (UsesVatNumberChanged useVatNumber |> dispatch))
            }
        ]
        div [ Style [ MaxWidth "225px" ] ] [
            if not state.Organization.UsesVatNumber then
                formGroup [
                    Label "Ondernemingsnr."
                    Input [ 
                        Type "text"
                        Pattern "[0-9]{4}\.[0-9]{3}\.[0-9]{3}"
                        MaxLength 12.0
                        Placeholder "xxxx.xxx.xxx"
                        Helpers.valueOrDefault state.Organization.OrganizationNumber
                        OnChange (fun e -> OrganizationNumberChanged e.Value |> dispatch)
                    ]
                ]
            else
                formGroup [
                    Label "BTW nr."
                    Input [
                        Type "text"
                        MinLength 4.0
                        MaxLength 15.0
                        Helpers.valueOrDefault state.Organization.VatNumber
                        OnChange (fun e -> VatNumberChanged e.Value |> dispatch)
                    ]
                    InputAppend [
                        match state.Organization.VatNumberVerifiedOn with
                        | Some _ ->
                            yield
                                button [
                                    Type "button"
                                    classes [ Bootstrap.btn; Bootstrap.btnOutlineSuccess ]
                                ] [ 
                                    i [ classes [ FontAwesome.fa; FontAwesome.faThumbsUp ] ] [] 
                                ]
                        | None ->
                            match state.VatNumberCheckingState with
                            | Some (CheckingVatNumber) ->
                                yield
                                    button [ 
                                        Type "button"
                                        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ] 
                                    ] [
                                        i [ classes [ FontAwesome.fa; FontAwesome.faSpinner; FontAwesome.faSpin ] ] []
                                    ]
                            | Some (VatNumberCheckingFailed) ->
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
                                        OnClick (fun _ -> VerifyVatNumber |> dispatch)
                                    ] [
                                        i [ classes [ FontAwesome.fa; FontAwesome.faSearch ] ] []
                                        span [] [ str " Verifiëren" ]
                                    ]
                    ]
                ]
        ]
    ]
    |> React.fragment


let view (state: State) (dispatch: Message -> unit) =
    div [] [
        renderOrganizationNumber state dispatch
        formGroup [ 
            Label "Naam" 
            Input [ 
                Type "text"
                MaxLength 255.0
                Helpers.valueOrDefault state.Organization.Name 
                OnChange (fun e -> NameChanged e.Value |> dispatch)
                Required true
            ] 
        ]
        div [ Class Bootstrap.formGroup ] [
            label [ HtmlFor "OrganizationTypes" ] [ str "Types (bvb. leverancier, loodgieter, e.d.)" ]
            div [ 
                classes [ Bootstrap.formControl; Bootstrap.w100; "pointer" ]
                OnClick (fun _ -> AddOrganizationType |> dispatch)
                HTMLAttr.Name "OrganizationTypes"
            ] [
                if state.Organization.OrganizationTypes.Length > 0
                then
                    yield! state.Organization.OrganizationTypes |> List.map (renderOrganizationType dispatch)
                else
                    yield str "Selecteren"
            ]
        ]
        h4 [] [ str "Adres" ]
        AddressEditComponent.render state.Organization.Address (Some (fun a -> AddressChanged a |> dispatch)) (nameof state.Organization.Address) state.Errors
        formGroup [ 
            Label "Tel."
            Input [ 
                Type "tel"
                MaxLength 32.0 
                Helpers.valueOrDefault state.Organization.MainTelephoneNumber
                OnChange (fun e -> MainTelephoneNumberChanged e.Value |> dispatch)
            ] 
        ]
        formGroup [ 
            Label "Tel. commentaar"
            Input [ 
                Type "text"
                MaxLength 255.0 
                Helpers.valueOrDefault state.Organization.MainTelephoneNumberComment
                OnChange (fun e -> MainTelephoneNumberCommentChanged e.Value |> dispatch)
            ] 
        ]
        formGroup [ 
            Label "E-mail"
            Input [ 
                Type "email"
                MaxLength 255.0 
                Helpers.valueOrDefault state.Organization.MainEmailAddress
                OnChange (fun e -> MainEmailAddressChanged e.Value |> dispatch)
            ] 
        ]
        formGroup [ 
            Label "E-mail commentaar"
            Input [
                Type "text"
                MaxLength 255.0
                Helpers.valueOrDefault state.Organization.MainEmailAddressComment
                OnChange (fun e -> MainEmailAddressCommentChanged e.Value |> dispatch)
            ] 
        ]
        fieldset [] [
            legend [] [ h4 [] [ str "Andere contactmiddelen" ] ]
            yield! renderOtherContactMethods state.Organization.OtherContactMethods dispatch
        ]
        fieldset [] [
            legend [] [ h4 [] [ str "Contactpersonen" ] ]
            yield! renderContactPersons state.Organization.ContactPersons dispatch
        ]

        fieldset [] [
            legend [] [ h4 [] [ str "Bankrekeningen" ] ]
            BankAccountEditComponent.view state.Errors state.BankAccountEditComponentState (BankAccountEditComponentMsg >> dispatch)
        ]

        div [] [
            yield
                match state.ContactPersonModalState with
                | ContactPersonModalState.Closed -> null
                | Creating componentState -> renderCreateContactPersonModal componentState dispatch
                | Editing componentState -> renderEditContactPersonModal componentState dispatch
        ]
        div [] [
            yield
                match state.OrganizationTypeModalState with
                | Closed -> null
                | Opened selectedOrganizationTypes -> 
                    renderSelectOrganizationTypes 
                        selectedOrganizationTypes
                        state.AllOrganizationTypes
                        dispatch
        ]
    ]