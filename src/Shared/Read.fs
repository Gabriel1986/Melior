module Shared.Read

open System
open Library
open ConstrainedTypes

//Kavels -> toevoegen aandeel (= facturatie begrip) (= quotiteit volgens wetgeving)

//Verdeelsleutels ->
//	Vaste 10 verdeelsleutels

type GeneralMeetingPeriod = {
    FromDay: int
    FromMonth: int
    UntilDay: int
    UntilMonth: int
}

type CurrentUser = {
    UserId: Guid
    EmailAddress: string
    DisplayName: string
    PersonId: Guid
    Role: Role
    IsActive: bool
    BuildingIds: Guid list
    PreferredLanguageCode: string
}
and Role =
    | Owner
    | Syndic
    | SysAdmin

type Building = 
    {
        BuildingId: Guid
        IsActive: bool
        Code: string
        Name: string
        Address: Address
        OrganizationNumber: string option
        Remarks: string option
        GeneralMeetingPeriod: GeneralMeetingPeriod option
        Concierge: Concierge option
        Syndic: Syndic option
        YearOfConstruction: int option
        YearOfDelivery: int option
    }
    static member Init () = {
        BuildingId = Guid.NewGuid()
        IsActive = true
        Code = ""
        Name = ""
        Address = Address.Init
        OrganizationNumber = None
        Remarks = None
        GeneralMeetingPeriod = None
        Concierge = None
        Syndic = None
        YearOfConstruction = None
        YearOfDelivery = None
    }
    member me.ToListItem (): BuildingListItem = {
        BuildingId = me.BuildingId
        IsActive = me.IsActive
        Code = me.Code
        Name = me.Name
        Address = me.Address
        OrganizationNumber = me.OrganizationNumber
    }
and Address =
    {
        Street: string option
        ZipCode: string option
        Town: string option
        Country: string option
    }
    static member Init = { 
        Street = None
        ZipCode = None
        Town = None
        Country = Some "België" 
    }
    override me.ToString () =
        let street = defaultArg me.Street ""
        let zipCode = defaultArg me.ZipCode ""
        let town = defaultArg me.Town ""
        let country = defaultArg me.Country ""

        sprintf "%s, %s %s, %s" street zipCode town country
and OtherAddress = 
    {
        Name: string
        Description: string
        Address: Address
    }
    static member Init = {
        Name = ""
        Description = ""
        Address = Address.Init
    }

and Concierge =
    | Owner of Owner
    | NonOwner of Person
and ContactMethod = 
    {
        ContactMethodType: ContactMethodType
        Value: string
        Description: string
    }
    static member Init () = {
        ContactMethodType = ContactMethodType.PhoneNumber
        Value = ""
        Description = ""
    }
and ContactMethodType =
    | PhoneNumber
    | EmailAddress
    | WebSite
    | Other
and Syndic =
    | Owner of Owner
    | ProfessionalSyndic of ProfessionalSyndic
    | Other of Person

and BuildingListItem = {
    BuildingId: Guid
    IsActive: bool
    Code: string
    Name: string
    Address: Address
    OrganizationNumber: string option
}
//and BankAccount = {
//    CurrencyCode: string
//    BankAccountId: Guid
//    Number: string
//    Iban: string
//    Bic: string
//}

//Gemeenschappelijke ruimte
and CommonSpace = string list
and Lot = {
    LotId: Guid
    Building: {| BuildingId: Guid; Name: string |}
    CurrentOwner: LotOwner
    Code: string
    LotType: LotType
    Description: string option
    //Which floor this lot is on
    Floor: int
    //Surface in square metres, if necessary, could be calculated in square feet
    Surface: int
    IsActive: bool
}
and LotOwner =
    | Person of {| PersonId: Guid; FirstName: string; LastName: string |}
    | Organization of {| OrganizationId: Guid; Name: string |}
and LotType =
    | Appartment
    | Studio
    | ParkingSpace
    | CommercialProperty
    | Garage
    | Storage
    | Other
    override me.ToString() =
        match me with
        | Appartment -> "Appartement"
        | Studio -> "Studio"
        | ParkingSpace -> "Staanplaats"
        | CommercialProperty -> "Handelspand"
        | Garage -> "Garage"
        | Storage -> "Opslagruimte"
        | Other -> "Andere"
and LotListItem = {
    LotId: Guid
    Building: {| BuildingId: Guid; Name: string |}
    CurrentOwner: LotOwner
    Code: string
    LotType: LotType
    Floor: int
    Description: string option
    IsActive: bool
}

//A (non-building) organization
and Organization = {
    OrganizationId: Guid
    OrganizationNumber: OrganizationNumber
    IsActive: bool
    OrganizationType: OrganizationType
    Name: string
    Address: Address
    MainContactPerson: ContactPerson
    OtherContactPersons: ContactPerson list
}
and ContactPerson = {
    OrganizationId: Guid
    Person: Person
}
and OrganizationType = {
    OrganizationTypeId: Guid
    Name: string
}
and OrganizationListItem = {
    OrganizationId: Guid
    OrganizationNumber: OrganizationNumber
    OrganizationType: string
    Name: string
}
//A flesh and blood person
and Person = 
    {
        PersonId: Guid
        FirstName: string option
        LastName: string option
        LanguageCode: string option
        Gender: Gender
        //Sir, Madame, etc.
        Title: string option
        MainAddress: Address
        ContactAddress: ContactAddress
        OtherAddresses: OtherAddress list
        MainTelephoneNumber: string option
        MainTelephoneNumberComment: string option
        MainEmailAddress: string option
        MainEmailAddressComment: string option
        OtherContactMethods: ContactMethod list
    }
    static member Init () = {
        PersonId = Guid.NewGuid()
        FirstName = None
        LastName = None
        LanguageCode = Some "nl-BE"
        Gender = Male
        Title = None
        MainAddress = Address.Init
        ContactAddress = MainAddress
        OtherAddresses = []
        MainTelephoneNumber = None
        MainTelephoneNumberComment = None
        MainEmailAddress = None
        MainEmailAddressComment = None
        OtherContactMethods = []
    }
and ContactAddress =
    | MainAddress
    | ContactAddress of Address
and Gender =
    | Male
    | Female
    | Other
    static member FromString (str: string) =
        match str with
        | x when x = string Male   -> Male
        | x when x = string Female -> Female
        | _                        -> Other

and Owner = {
    BuildingId: Guid
    Person: Person
    IsActive: bool
    IsResident: bool
    //Lots: LotListItem list
}
and Tenant = {
    TenantId: Guid
    Person: Person
    IsActive: bool
    MoveInDate: DateTimeOffset
    MoveOutDate: DateTimeOffset option
    //Lots: LotListItem list
}
and ProfessionalSyndic = {
    ProfessionalSyndicId: Guid
    Person: Person
    IsActive: bool
}
and OwnerListItem = {
    BuildingId: Guid
    PersonId: Guid
    FirstName: string option
    LastName: string option
    IsResident: bool
    IsActive: bool
}
and TenantListItem = {
    TenantId: Guid
    FirstName: string
    LastName: string
    IsActive: bool
    MoveInDate: DateTimeOffset
    MoveOutDate: DateTimeOffset option
}
and ProfessionalSyndicListItem = {
    ProfessionalSyndicId: Guid
    FirstName: string option
    LastName: string option
    IsActive: bool
}