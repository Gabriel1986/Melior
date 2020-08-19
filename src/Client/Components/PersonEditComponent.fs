module Client.Components.PersonEditComponent

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props

open Client.ClientStyle
open Client.ClientStyle.Helpers
open Shared.Read
open Shared.Library

type State = {
    Person: Person
    CreateOrUpdate: CreateOrUpdate
    Errors: (string * string) list
}

type Message =
    | FirstNameChanged of string
    | LastNameChanged of string
    | LanguageCodeChanged of string
    | GenderChanged of Gender
    | TitleChanged of string
    | MainAddressChanged of Address
    | ContactAddressChanged of ContactAddress
    | OtherAddressAdded
    | OtherAddressRemoved of index: int
    | OtherAddressDescriptionChanged of index: int * newName: string
    | OtherAddressAddressChanged of index: int * newAddress: Address
    | MainTelephoneNumberChanged of string
    | MainTelephoneNumberCommentChanged of string
    | MainEmailAddressChanged of string
    | MainEmailAddressCommentChanged of string
    | OtherContactMethodAdded
    | OtherContactMethodRemoved of index: int
    | OtherContactMethodDescriptionChanged of index: int * newDescription: string
    | OtherContactMethodTypeChanged of index: int * newType: ContactMethodType
    | OtherContactMethodValueChanged of index: int * newValue: string

let init (person: Person option) =
    let person, createOrUpdate =
        match person with
        | Some person -> person, Update
        | None        -> Person.Init (), Create
    { 
        Person = person
        CreateOrUpdate = createOrUpdate
        Errors  = [] 
    }, Cmd.none

let update (message: Message) (state: State): State * Cmd<Message> =
    let changePerson alterFunc =
        { state with Person = alterFunc state.Person }

    let changeOtherAddresses otherAddresses =
        changePerson (fun p -> { p with OtherAddresses = otherAddresses })

    let changeOtherContactMethods otherContactMethods =
        changePerson (fun p -> { p with OtherContactMethods = otherContactMethods })

    match message with
    | FirstNameChanged x ->
        changePerson (fun p -> { p with FirstName = x |> String.toOption }), Cmd.none
    | LastNameChanged x ->
        changePerson (fun p -> { p with LastName = x |> String.toOption }), Cmd.none
    | LanguageCodeChanged x ->
        changePerson (fun p -> { p with LanguageCode = x |> String.toOption }), Cmd.none
    | GenderChanged x ->
        changePerson (fun p -> { p with Gender = x }), Cmd.none
    | TitleChanged x ->
        changePerson (fun p -> { p with Title = x |> String.toOption }), Cmd.none
    | MainAddressChanged x ->
        changePerson (fun p -> { p with MainAddress = x }), Cmd.none
    | ContactAddressChanged x ->
        changePerson (fun p -> { p with ContactAddress = x }), Cmd.none
    | OtherAddressAdded ->
        let newOtherAddresses = (OtherAddress.Init ())::state.Person.OtherAddresses
        changeOtherAddresses newOtherAddresses, Cmd.none
    | OtherAddressRemoved otherAddressIndex ->
        let newOtherAddresses = 
            state.Person.OtherAddresses 
            |> List.indexed 
            |> List.choose (fun (index, a) -> if otherAddressIndex = index then None else Some a)
        changeOtherAddresses newOtherAddresses, Cmd.none
    | OtherAddressDescriptionChanged (otherAddressIndex, newDescription) ->
        let newOtherAddresses = state.Person.OtherAddresses |> List.mapi (fun index a ->
            if index = otherAddressIndex then { a with Description = newDescription } else a
        )
        changeOtherAddresses newOtherAddresses, Cmd.none
    | OtherAddressAddressChanged (otherAddressIndex, newAddress) ->
        let newOtherAddresses = state.Person.OtherAddresses |> List.mapi (fun index a ->
            if index = otherAddressIndex then { a with Address = newAddress } else a
        )
        changeOtherAddresses newOtherAddresses, Cmd.none
    | MainTelephoneNumberChanged x ->
        changePerson (fun p -> { p with MainTelephoneNumber = x |> String.toOption }), Cmd.none
    | MainTelephoneNumberCommentChanged x ->
        changePerson (fun p -> { p with MainTelephoneNumberComment = x |> String.toOption }), Cmd.none
    | MainEmailAddressChanged x ->
        changePerson (fun p -> { p with MainEmailAddress = x |> String.toOption }), Cmd.none
    | MainEmailAddressCommentChanged x ->
        changePerson (fun p -> { p with MainEmailAddressComment = x |> String.toOption }), Cmd.none
    | OtherContactMethodAdded ->
        let newOtherContactMethods = (ContactMethod.Init())::state.Person.OtherContactMethods
        changeOtherContactMethods newOtherContactMethods, Cmd.none
    | OtherContactMethodRemoved cmIndex ->
        let newOtherContactMethods = 
            state.Person.OtherContactMethods 
            |> List.indexed
            |> List.choose (fun (index, cm) -> if index = cmIndex then None else Some cm)
        changeOtherContactMethods newOtherContactMethods, Cmd.none
    | OtherContactMethodDescriptionChanged (cmIndex, newDescription) ->
        let newOtherContactMethods = state.Person.OtherContactMethods |> List.mapi (fun index cm ->
            if index = cmIndex then { cm with Description = newDescription } else cm
        )
        changeOtherContactMethods newOtherContactMethods, Cmd.none
    | OtherContactMethodTypeChanged (cmIndex, newType) ->
        let newOtherContactMethods = state.Person.OtherContactMethods |> List.mapi (fun index cm ->
            if index = cmIndex then { cm with ContactMethodType = newType } else cm
        )
        changeOtherContactMethods newOtherContactMethods, Cmd.none
    | OtherContactMethodValueChanged (cmId, newValue) ->
        let newOtherContactMethods = state.Person.OtherContactMethods |> List.mapi (fun index cm ->
            if index = cmId then { cm with Value = newValue } else cm
        )
        changeOtherContactMethods newOtherContactMethods, Cmd.none

let private genderOptions dispatch currentGender: FormRadioButton list =
    let onClick = (fun gender ->
        let newGender =
            match gender with
            | x when x = string Male -> Male
            | x when x = string Female -> Female
            | _ -> Other
        GenderChanged newGender |> dispatch
    )
    [
        {
            Id = "maleGender"
            Key = string Male
            Label = "M"
            IsSelected = currentGender = Male
            OnClick = onClick
        }
        {
            Id = "femaleGender"
            Key = string Female
            Label = "V"
            IsSelected = currentGender = Female
            OnClick = onClick
        }
        {
            Id = "otherGender"
            Key = string Other
            Label = "Ander"
            IsSelected = currentGender = Other
            OnClick = onClick
        }
    ]

let private languageOptions dispatch currentLanguageCode: FormRadioButton list = 
    let onClick = (fun newLanguageCode -> LanguageCodeChanged newLanguageCode |> dispatch)
    let currentLanguageCode' = currentLanguageCode |> Option.defaultValue "nl-BE"
    [
        {
            Id = "dutchLanguage"
            Key = "nl-BE"
            Label = "Nederlands"
            IsSelected = currentLanguageCode' = "nl-BE"
            OnClick = onClick
        }
        {
            Id = "frenchLanguage"
            Key = "fr-BE"
            Label = "Frans"
            IsSelected = currentLanguageCode' = "fr-BE"
            OnClick = onClick
        }
        {
            Id = "englishLanguage"
            Key = "en-GB"
            Label = "Engels"
            IsSelected = currentLanguageCode' = "en-GB"
            OnClick = onClick
        }
        {
            Id = "otherLanguage"
            Key = "Other"
            Label = "Other"
            IsSelected = currentLanguageCode' = "Other"
            OnClick = onClick
        }
    ]

let private renderAddress = AddressEditComponent.render

let private renderMainAddress (state: State) dispatch =
    renderAddress 
        "Hoofdadres" 
        state.Person.MainAddress 
        (MainAddressChanged >> dispatch)
        (nameof state.Person.MainAddress)
        state.Errors

let private renderContactAddress (state: State) dispatch =
    let contactAddress = state.Person.ContactAddress
    let yesNo = [
        {
            Id = "contactAddressSame"
            Key = "same"
            Label = "Ja"
            IsSelected = match contactAddress with | MainAddress -> true | ContactAddress _ -> false
            OnClick = (fun _ -> ContactAddressChanged MainAddress |> dispatch)
        }
        {
            Id = "contactAddressNotSame"
            Key = "notSame"
            Label = "Nee"
            IsSelected = match contactAddress with | MainAddress -> false | ContactAddress _ -> true
            OnClick = (fun _ -> ContactAddressChanged (ContactAddress (Address.Init ())) |> dispatch)
        }
    ]

    [
        yield formGroup [ Label "Contactadres zelfde als hoofdadres?"; Radio { Inline = true; RadioButtons = yesNo } ]
        match contactAddress with
        | ContactAddress addr -> 
            yield 
                renderAddress 
                    "Contact adres" 
                    addr 
                    (ContactAddress >> ContactAddressChanged >> dispatch)
                    (nameof state.Person.ContactAddress)
                    state.Errors
        | MainAddress -> 
            ()
    ]

let private renderOtherAddresses (state: State) dispatch =
    let addresses = state.Person.OtherAddresses
    [
        yield! addresses |> List.mapi (fun index a ->
            div [] [
                formGroup [ 
                    Label a.Description
                    Input [ 
                        Type "text"
                        Helpers.valueOrDefault a.Description
                        OnChange (fun e -> OtherAddressDescriptionChanged (index, e.Value) |> dispatch) 
                    ]
                ]
                button [ classes [ Bootstrap.btn; Bootstrap.btnPrimary ]; OnClick (fun _ -> OtherAddressRemoved index |> dispatch) ] [
                    i [ classes [ FontAwesome.fa; FontAwesome.faXing ] ] []
                ]
                renderAddress 
                    a.Description 
                    a.Address 
                    (fun newA -> OtherAddressAddressChanged (index, newA) |> dispatch)
                    (sprintf "%s.[%i]"  (nameof (state.Person.OtherAddresses)) index)
                    state.Errors
            ]
        )
        yield 
            button [ classes [ Bootstrap.btn; Bootstrap.btnPrimary ]; OnClick (fun _ -> OtherAddressAdded |> dispatch) ] [
                i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                str " "
                str "Ander adres toevoegen"
            ]
    ]

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
            div [] [
                formGroup [ 
                    Label c.Description
                    Input [ 
                        Type "text"
                        MaxLength 255.0
                        Helpers.valueOrDefault c.Description
                        OnChange (fun e -> OtherContactMethodDescriptionChanged (index, e.Value) |> dispatch) 
                    ]
                ]
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
                formGroup [
                    Label "Value"
                    Input [
                        Type "text"
                        Helpers.valueOrDefault c.Value
                        OnChange (fun e -> OtherContactMethodValueChanged (index, e.Value) |> dispatch) 
                    ]
                ]
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
                    OnClick (fun _ -> OtherContactMethodRemoved index |> dispatch) 
                ] [
                    str "Verwijderen"
                ]
            ]
        )
        yield
            button [ classes [ Bootstrap.btn; Bootstrap.btnPrimary ]; OnClick (fun _ -> OtherContactMethodAdded |> dispatch) ] [
                i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                str " "
                str "Ander contactmiddel toevoegen"
            ]
    ]

let view state dispatch =
    div [] [
        yield formGroup [ 
            Label "Voornaam" 
            Input [ 
                Type "text"
                MaxLength 255.0
                Helpers.valueOrDefault state.Person.FirstName 
                OnChange (fun e -> FirstNameChanged e.Value |> dispatch)
                Required true
            ] 
        ]
        yield formGroup [ 
            Label "Achternaam"
            Input [ 
                Type "text"
                MaxLength 255.0
                Helpers.valueOrDefault state.Person.LastName
                OnChange (fun e -> LastNameChanged e.Value |> dispatch)
                Required true
            ] 
        ]
        yield formGroup [ 
            Label "Taal"
            Radio { 
                Inline = true
                RadioButtons = languageOptions dispatch state.Person.LanguageCode 
            } 
        ]
        yield formGroup [ 
            Label "Geslacht"
            Radio { 
                Inline = true
                RadioButtons = genderOptions dispatch state.Person.Gender
            } 
        ]
        yield formGroup [ 
            Label "Aanhef"
            Input [ 
                Type "text"
                MaxLength 32.0
                Helpers.valueOrDefault state.Person.Title
                OnChange (fun e -> TitleChanged e.Value |> dispatch)
            ] 
        ]
        yield renderMainAddress state dispatch
        yield! renderContactAddress state dispatch
        yield! renderOtherAddresses state dispatch
        yield formGroup [ 
            Label "Tel."
            Input [ 
                Type "tel"
                MaxLength 32.0 
                Helpers.valueOrDefault state.Person.MainTelephoneNumber
                OnChange (fun e -> MainTelephoneNumberChanged e.Value |> dispatch)
            ] 
        ]
        yield formGroup [ 
            Label "Tel. commentaar"
            Input [ 
                Type "text"
                MaxLength 255.0 
                Helpers.valueOrDefault state.Person.MainTelephoneNumberComment
                OnChange (fun e -> MainTelephoneNumberCommentChanged e.Value |> dispatch)
            ] 
        ]
        yield formGroup [ 
            Label "E-mail"
            Input [ 
                Type "email"
                MaxLength 255.0 
                Helpers.valueOrDefault state.Person.MainEmailAddress
                OnChange (fun e -> MainEmailAddressChanged e.Value |> dispatch)
            ] 
        ]
        yield formGroup [ 
            Label "E-mail commentaar"
            Input [
                Type "text"
                MaxLength 255.0
                Helpers.valueOrDefault state.Person.MainEmailAddressComment
                OnChange (fun e -> MainEmailAddressCommentChanged e.Value |> dispatch)
            ] 
        ]
        yield! renderOtherContactMethods state.Person.OtherContactMethods dispatch
    ]