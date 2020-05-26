module Server.Persons.Workflow

open Shared.Domain
open Server.Persons.Storage
open Shared.ConstrainedTypes
open Shared
open Shared.Library
open Shared.Trial.Control
open Server.Addresses.Workflow
open Server.ContactMethods.Workflow
open Server.Library
open Shared.Remoting

let private validateContactAddress (contactAddress: ContactAddress) =
    match contactAddress with
    | MainAddress -> Trial.Pass None
    | ContactAddress c -> validateAddress c |> Trial.map Some

let private validateOtherAddress (otherAddress: OtherAddress) =
    trial {
        from name in String255.Of otherAddress.Name
        also address in validateAddress otherAddress.Address
        yield {
            Name = name
            Description = otherAddress.Description
            Address = address
        }
    }

let private validatePerson (person: Person) = trial {
    from firstName in validateOptional String255.Of person.FirstName
    also lastName in validateOptional String255.Of person.LastName
    also languageCode in validateOptional String16.Of person.LanguageCode
    also title in validateOptional String32.Of person.Title
    also mainTelephoneNumber in validateOptional String32.Of person.MainTelephoneNumber
    also mainTelephoneNumberComment in validateOptional String255.Of person.MainTelephoneNumberComment
    also mainEmailAddress in validateOptional String255.Of person.MainEmailAddress
    also mainEmailAddressComment in validateOptional String255.Of person.MainEmailAddressComment
    also mainAddress in validateAddress person.MainAddress
    also contactAddress in validateContactAddress person.ContactAddress
    also otherContactMethods in person.OtherContactMethods |> List.map (fun c -> validateContactMethod c) |> Trial.sequence
    also otherAddresses in person.OtherAddresses |> List.map (fun a -> validateOtherAddress a) |> Trial.sequence
    yield {
        PersonId = person.PersonId        
        FirstName = firstName
        LastName = lastName
        LanguageCode = languageCode
        Gender = person.Gender
        Title = title
        MainAddress = mainAddress
        ContactAddress = contactAddress
        OtherAddresses = otherAddresses |> List.ofSeq
        MainTelephoneNumber = mainTelephoneNumber
        MainTelephoneNumberComment = mainTelephoneNumberComment
        MainEmailAddress = mainEmailAddress
        MainEmailAddressComment = mainEmailAddressComment
        OtherContactMethods = otherContactMethods |> List.ofSeq
    }
}

let createPerson (connectionString: string) (msg: Message<Person>): Async<Result<unit, CreatePersonError>> = async {
    let result = validatePerson msg.Payload
    match result with
    | Trial.Pass person ->
        do! Server.Persons.Storage.createPerson connectionString person
        return Ok ()
    | Trial.Fail errors ->
        return Error
            (CreatePersonError.ValidationError {
                GeneralErrors = errors
                FirstNameErrors = []
                LastNameErrors = []
                LanguageCodeErrors = []
                TitleErrors = []
                MainTelephoneNumberErrors = []
                MainTelephoneNumberCommentErrors = []
                MainEmailAddressErrors = []
                MainEmailAddressCommentErrors = []
                MainAddressErrors = []
                ContactAddressErrors = []
                OtherContactMethodsErrors = []
                OtherAddressesErrors = []
            })
}

let updatePerson (connectionString: string) (msg: Message<Person>): Async<Result<unit, UpdatePersonError>> = async {
    let result = validatePerson msg.Payload
    match result with
    | Trial.Pass person ->
        do! Server.Persons.Storage.updatePerson connectionString person
        return Ok ()
    | Trial.Fail errors ->
        return Error
            (UpdatePersonError.ValidationError {
                GeneralErrors = errors
                FirstNameErrors = []
                LastNameErrors = []
                LanguageCodeErrors = []
                TitleErrors = []
                MainTelephoneNumberErrors = []
                MainTelephoneNumberCommentErrors = []
                MainEmailAddressErrors = []
                MainEmailAddressCommentErrors = []
                MainAddressErrors = []
                ContactAddressErrors = []
                OtherContactMethodsErrors = []
                OtherAddressesErrors = []
            })
}