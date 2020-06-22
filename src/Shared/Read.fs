module Shared.Read

open System

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
and Lot = 
    {
        LotId: Guid
        BuildingId: Guid
        CurrentOwner: LotOwner option
        Code: string
        LotType: LotType
        Description: string option
        //Which floor this lot is on
        Floor: int option
        //Surface in square metres, if necessary, could be calculated in square feet
        Surface: int option
    }
    static member Init (buildingId: Guid) = {
        LotId = Guid.NewGuid()
        BuildingId = buildingId
        CurrentOwner = None
        Code = ""
        LotType = LotType.Appartment
        Description = None
        Floor = None
        Surface = None
    }
and LotOwner = 
    | Owner of Owner
    | Organization of Organization
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
    static member OfString str =
        match str with
        | _ when str = string Appartment -> Appartment
        | _ when str = string Studio -> Studio
        | _ when str = string ParkingSpace -> ParkingSpace
        | _ when str = string CommercialProperty -> CommercialProperty
        | _ when str = string Garage -> Garage
        | _ when str = string Storage -> Storage
        | _ -> LotType.Other

and LotListItem = {
    LotId: Guid
    BuildingId: Guid
    CurrentOwner: LotOwnerListItem option
    Code: string
    LotType: LotType
    Floor: int option
    Description: string option
}
and LotOwnerListItem =
    | Person of {| PersonId: Guid; Name: string |}
    | Organization of {| OrganizationId: Guid; Name: string |}

//A (non-building) organization
and Organization = 
    {
        OrganizationId: Guid
        BuildingId: Guid option
        OrganizationNumber: string option
        VatNumber: string option
        VatNumberVerifiedOn: DateTime option
        OrganizationTypes: OrganizationType list
        Name: string
        Address: Address
        MainEmailAddress: string option
        MainEmailAddressComment: string option
        MainTelephoneNumber: string option
        MainTelephoneNumberComment: string option
        OtherContactMethods: ContactMethod list
        ContactPersons: ContactPerson list
    }
    static member Init (buildingId: Guid option): Organization = 
        let orgId = Guid.NewGuid()
        {
            OrganizationId = orgId
            BuildingId = buildingId
            OrganizationNumber = None
            VatNumber = None
            VatNumberVerifiedOn = None
            OrganizationTypes = []
            Name = ""
            Address = Address.Init
            MainEmailAddress = None
            MainEmailAddressComment = None
            MainTelephoneNumber = None
            MainTelephoneNumberComment = None
            OtherContactMethods = []
            ContactPersons = []
        }
and ContactPerson = 
    {
        OrganizationId: Guid
        Person: Person
        RoleWithinOrganization: string
    }
    static member Init (orgId: Guid) = {
        OrganizationId = orgId
        Person = Person.Init ()
        RoleWithinOrganization = ""
    }
and OrganizationType = 
    {
        OrganizationTypeId: Guid
        Name: string
    }
    static member Init () = {
        OrganizationTypeId = Guid.NewGuid()
        Name = ""
    }
and OrganizationListItem = {
    OrganizationId: Guid
    BuildingId: Guid option
    OrganizationNumber: string option
    OrganizationTypeNames: string list
    Name: string
    Address: Address
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
    member me.FullName = sprintf "%s %s" (defaultArg me.FirstName "") (defaultArg me.LastName "")
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

and Owner = 
    {
        BuildingId: Guid
        Person: Person
        IsResident: bool
    }
    static member Init (buildingId: Guid) = {
        BuildingId = buildingId
        Person = Person.Init ()
        IsResident = true
    }
and Tenant = {
    Person: Person
    MoveInDate: DateTimeOffset
    MoveOutDate: DateTimeOffset option
}
and ProfessionalSyndic = 
    {
        Organization: Organization
    }
    static member Init () = {
        Organization = Organization.Init None
    }
and OwnerListItem = {
    BuildingId: Guid
    PersonId: Guid
    FirstName: string option
    LastName: string option
    IsResident: bool
}
and TenantListItem = {
    PersonId: Guid
    FirstName: string
    LastName: string
    MoveInDate: DateTimeOffset
    MoveOutDate: DateTimeOffset option
}
and ProfessionalSyndicListItem = {
    OrganizationId: Guid
    Name: string
    Address: Address
    MainEmailAddress: string option
    MainTelephoneNumber: string option
}

type VatNumberValidationResponse = {
    CountryCode: string
    VatNumber: string
    RequestDate: DateTimeOffset
    IsValid: bool
    Name: string option
    Address: string option
}
