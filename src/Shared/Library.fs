namespace Shared

open System

module Library =
    type Building = {
        BuildingId: Guid
        IsActive: bool
        Code: string option
        Name: string
        Address: Address
        OrganizationNumber: string option
        Remarks: string
        GeneralMeetingFrom: DateTimeOffset
        GeneralMeetingUntil: DateTimeOffset
        Concierge: Concierge option
        YearOfConstruction: int
        LinkedLots: LotListItem []
        Responsible: Responsible
        OtherLinkedResidents: ResidentListItem []
    }
    and Address = {
        Street: string
        ZipCode: string
        Town: string
    }
    and Concierge = {
        ConciergeId: Guid
        Name: string
        MainContactMethod: ContactMethod
        OtherContactMethods: ContactMethod []
        Remarks: string
    }
    and ContactMethod = {
        ContactMethodId: Guid
        ContactMethodType: ContactMethodType
        IsPrivate: bool
    }
    and ContactMethodType =
        | PhoneNumber of string
        | EmailAddress of string
        | WebSite of string
    and Responsible =
        | Resident of ResidentListItem
        | Syndic of SyndicListItem

    and BuildingListItem = {
        BuildingId: Guid
        IsActive: bool
        Code: string option
        Name: string
        Address: Address
        OrganizationNumber: string
    }
    and BankAccount = {
        BankAccountId: Guid
        Number: string
        Iban: string
        Bic: string
    }

    and Lot = {
        LotId: Guid
        BuildingId: Guid
        LotType: LotType
        Description: string
        //Which floor this lot is on
        Floor: int
        //Surface in square metres, if necessary, could be calculated in square feet
        Surface: int
        IsActive: bool
        LinkedResidents: ResidentListItem []
    }
    and LotType =
        | Appartment
        | ParkingSpace
        | Store
        | Garage
        | Basement
        | Other
    and LotListItem = {
        LotId: Guid
        BuildingId: Guid
        LotType: LotType
        Description: string
        IsActive: bool
    }

    //A (non-building) organization
    and Organization = {
        OrganizationNumber: string
        IsActive: bool
        OrganizationType: OrganizationType
        Name: string
        BankAccount: BankAccount option
        MainContactMethod: ContactMethod option
        OtherContactMethods: ContactMethod []
        LinkedResidents: ResidentListItem []
    }
    and OrganizationType = {
        OrganizationTypeId: Guid
        Name: string
    }
    and OrganizationListItem = {
        OrganizationNumber: string
        OrganizationType: OrganizationType
        Name: string
    }
    //A flesh and blood person
    and Person = {
        PersonId: Guid
        FirstName: string
        LastName: string
        Language: Language
        Gender: Gender
        Address: Address
        MainContactMethod: ContactMethod
        OtherContactMethods: ContactMethod []
        CountryCode: string
        CurrencyCode: string
        BankAccount: BankAccount
        LinkedOrganizations: OrganizationListItem []
    }
    and Gender =
        | Male
        | Female of married: bool
    and Language = {
        LanguageId: Guid
        Name: string
        //ISO 639-1 Code
        Code: string
    }
    //Inhabitant of a building
    and Resident = {
        Person: Person
        BuildingId: Guid
        IsActive: bool
        MovedInDate: DateTimeOffset
        MovedOutDate: DateTimeOffset option
        LinkedLots: LotListItem []
    }
    and ResidentListItem = {
        BuildingId: Guid
        PersonId: Guid
        FirstName: string
        LastName: string
        IsActive: bool
        MovedInDate: DateTimeOffset
        MovedOutDate: DateTimeOffset option
    }
    and SyndicListItem = {
        BuildingId: Guid
        PersonId: Guid
        FirstName: string
        LastName: string
        StartDate: DateTimeOffset
        EndDate: DateTimeOffset option
    }