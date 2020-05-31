module Shared.Write

open System
open Read
open ConstrainedTypes
open Library
open Trial.Control

type ValidatedAddress = {
    Street: String255 option
    ZipCode: String16 option
    Town: String64 option
    Country: String64 option
}

let private validateAddress (basePath: string) (address: Address) =
    let createPath s = sprintf "%s.%s" basePath s
    trial {
        from street in validateOptional (String255.Of (createPath (nameof address.Street))) address.Street
        also zipCode in validateOptional (String16.Of (createPath (nameof address.ZipCode))) address.ZipCode
        also town in validateOptional (String64.Of (createPath (nameof address.Town))) address.Town
        also country in validateOptional (String64.Of (createPath (nameof address.Country))) address.Country
        yield {
            Street = street
            ZipCode = zipCode
            Town = town
            Country = country
        }
    }

type ValidatedOtherAddress = 
    {
        Name: String255
        Description: string
        Address: ValidatedAddress
    }

let private validateOtherAddress (basePath: string) (otherAddress: OtherAddress) = trial {
    from name in String255.Of (sprintf "%s.%s" basePath (nameof otherAddress.Name)) otherAddress.Name
    also address in validateAddress (sprintf "%s.%s" basePath (nameof otherAddress.Address)) otherAddress.Address
    yield {
        Name = name
        Description = otherAddress.Description
        Address = address
    }
}

type ValidatedContactMethod = 
    {
        ContactMethodType: ContactMethodType
        Value: String255
        Description: string
    }

let private validateContactMethod (basePath: string) (contactMethod: ContactMethod) = trial {
    from value in String255.Of (sprintf "%s.%s" basePath (nameof contactMethod.Value)) contactMethod.Value
    yield {
        ContactMethodType = contactMethod.ContactMethodType
        Value = value
        Description = contactMethod.Description
    }
}

let private validateContactAddress (path: string) (contactAddress: ContactAddress) =
    match contactAddress with
    | MainAddress -> Trial.Pass None
    | ContactAddress c -> validateAddress path c |> Trial.map Some


type ValidatedPerson = 
    {
        PersonId: Guid
        FirstName: String255 option
        LastName: String255 option
        LanguageCode: String16 option
        Gender: Gender
        Title: String32 option
        MainAddress: ValidatedAddress
        ContactAddress: ValidatedAddress option
        OtherAddresses: ValidatedOtherAddress list
        MainTelephoneNumber: String32 option
        MainTelephoneNumberComment: String255 option
        MainEmailAddress: String255 option
        MainEmailAddressComment: String255 option
        OtherContactMethods: ValidatedContactMethod list
    }
    static member Validate (person: Person) =
        trial {
           from firstName in validateOptional (String255.Of (nameof person.FirstName)) person.FirstName
           also lastName in validateOptional (String255.Of (nameof person.LastName)) person.LastName
           also languageCode in validateOptional (String16.Of (nameof person.LanguageCode)) person.LanguageCode
           also title in validateOptional (String32.Of (nameof person.Title)) person.Title
           also mainTelephoneNumber in validateOptional (String32.Of (nameof person.MainTelephoneNumber)) person.MainTelephoneNumber
           also mainTelephoneNumberComment in validateOptional (String255.Of (nameof person.MainTelephoneNumberComment)) person.MainTelephoneNumberComment
           also mainEmailAddress in validateOptional (String255.Of (nameof person.MainEmailAddress)) person.MainEmailAddress
           also mainEmailAddressComment in validateOptional (String255.Of (nameof person.MainEmailAddressComment))  person.MainEmailAddressComment
           also mainAddress in validateAddress (nameof person.MainAddress) person.MainAddress
           also contactAddress in validateContactAddress (nameof person.ContactAddress) person.ContactAddress
           also otherContactMethods in person.OtherContactMethods |> List.mapi (fun index c -> validateContactMethod (sprintf "%s.[%i]" (nameof person.OtherContactMethods) index) c) |> Trial.sequence
           also otherAddresses in person.OtherAddresses |> List.mapi (fun index a -> validateOtherAddress (sprintf "%s.[%i]" (nameof person.OtherAddresses) index) a) |> Trial.sequence
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
        |> Trial.toResult


type SyndicId =
    | OwnerId of Guid
    | ProfessionalSyndicId of Guid
    | OtherId of Guid

type ConciergeId =
    | OwnerId of Guid
    | NonOwnerId of Guid

let private mapSyndicToSyndicId =
    function
    | Syndic.ProfessionalSyndic prof -> 
        SyndicId.ProfessionalSyndicId prof.ProfessionalSyndicId
    | Syndic.Owner owner ->
        SyndicId.OwnerId owner.Person.PersonId
    | Syndic.Other person ->
        SyndicId.OtherId person.PersonId

let private mapConciergeToConciergeId =
    function
    | Concierge.Owner owner ->
        ConciergeId.OwnerId owner.Person.PersonId
    | Concierge.NonOwner person ->
        ConciergeId.NonOwnerId person.PersonId

type ValidatedBuilding = 
    {
        BuildingId: Guid
        Code: String16
        Name: String255
        Address: ValidatedAddress
        OrganizationNumber: OrganizationNumber option
        Remarks: string option
        GeneralMeetingPeriod: GeneralMeetingPeriod option
        SyndicId: SyndicId option
        ConciergeId: ConciergeId option
        YearOfConstruction: PositiveInt option
        YearOfDelivery: PositiveInt option
    }
    static member Validate (building: Building): Result<ValidatedBuilding, (string * string) list> =
        trial {
            from code in String16.Of (nameof building.Code) building.Code
            also name in String255.Of (nameof building.Name) building.Name
            also yearOfConstruction in validateOptional (PositiveInt.Of (nameof building.YearOfConstruction)) building.YearOfConstruction
            also yearOfDelivery in validateOptional (PositiveInt.Of (nameof building.YearOfDelivery))  building.YearOfDelivery
            also address in validateAddress (nameof building.Address) building.Address
            also orgNr in validateOptional (OrganizationNumber.OfString (nameof building.OrganizationNumber)) building.OrganizationNumber
            yield {
                BuildingId = building.BuildingId
                Code = code
                Name = name
                Address = address
                OrganizationNumber = orgNr
                Remarks = building.Remarks
                GeneralMeetingPeriod = building.GeneralMeetingPeriod
                SyndicId = building.Syndic |> Option.map mapSyndicToSyndicId
                ConciergeId = building.Concierge |> Option.map mapConciergeToConciergeId
                YearOfConstruction = yearOfConstruction
                YearOfDelivery = yearOfDelivery
            }
        }
        |> Trial.toResult


type LotOwnerId =
    | OwnerId of Guid
    | OrganizationId of Guid

let private mapLotOwner =
    function
    | LotOwner.Owner owner -> OwnerId owner.Person.PersonId
    | LotOwner.Organization organization -> OrganizationId organization.OrganizationId

type ValidatedLot = 
    {
        LotId: Guid
        BuildingId: Guid
        CurrentOwnerId: LotOwnerId option
        Code: String16
        LotType: LotType
        Description: string option
        Floor: int option //Floor can be negative, it's only constrained in range
        Surface: PositiveInt option
        IsActive: bool
    }
    static member Validate (lot: Lot) = 
        trial {
            from code in String16.Of (nameof lot.Code) lot.Code
            also surface in validateOptional (PositiveInt.Of (nameof lot.Surface)) lot.Surface
            yield {
                LotId = lot.LotId
                BuildingId = lot.BuildingId
                CurrentOwnerId = lot.CurrentOwner |> Option.map mapLotOwner
                Code = code
                LotType = lot.LotType
                Description = lot.Description
                Floor = lot.Floor
                Surface = surface
                IsActive = lot.IsActive
            }
        }
        |> Trial.toResult