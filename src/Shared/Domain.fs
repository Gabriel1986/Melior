namespace Shared

open System

module Domain =
    type CurrentUser = {
        UserId: Guid
        EmailAddress: string
        DisplayName: string
        PersonId: Guid
        Role: Role
        BuildingIds: Guid list
    }
    and Role =
        | Resident
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
            GeneralMeetingFrom: DateTimeOffset option
            GeneralMeetingUntil: DateTimeOffset option
            Concierge: Concierge option
            Syndic: Syndic option
        }
        static member Init buildingId = {
            BuildingId = buildingId
            IsActive = true
            Code = ""
            Name = ""
            Address = Address.Init
            OrganizationNumber = None
            Remarks = None
            GeneralMeetingFrom = None
            GeneralMeetingUntil = None
            Concierge = None
            Syndic = None
        }
    and Address =
        {
            Street: string
            ZipCode: string
            Town: string
        }
        static member Init = { Street = ""; ZipCode = ""; Town = "" }
    and Concierge =
        | Resident of Resident
        | NonResident of Person
    and ContactMethod = {
        ContactMethodId: Guid
        ContactMethodType: ContactMethodType
        IsPrivate: bool
        Description: string
    }
    and ContactMethodType =
        | PhoneNumber of string
        | EmailAddress of string
        | WebSite of string
    and Syndic =
        | Resident of Resident
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
    and BankAccount = {
        CurrencyCode: string
        BankAccountId: Guid
        Number: string
        Iban: string
        Bic: string
    }

    and Lot = {
        LotId: Guid
        Building: {| BuildingId: Guid; Code: string |}
        Code: string
        LotType: LotType
        Description: string option
        //Which floor this lot is on
        Floor: int
        //Surface in square metres, if necessary, could be calculated in square feet
        Surface: int
        IsActive: bool
    }
    and LotType =
        | Appartment
        | ParkingSpace
        | Store
        | PlaceOfBusiness
        | Garage
        | Basement
        | Storage
        | Other
        override me.ToString() =
            match me with
            | Appartment -> "Appartement"
            | ParkingSpace -> "Parkeerplaats"
            | Store -> "Winkel"
            | PlaceOfBusiness -> "Bedrijfsruimte"
            | Garage -> "Garage"
            | Basement -> "Kelder"
            | Storage -> "Opslagruimte"
            | Other -> "Andere"
    and LotListItem = {
        LotId: Guid
        Building: {| BuildingId: Guid; Code: string |}
        Code: string
        LotType: LotType
        Floor: int
        Description: string option
        IsActive: bool
    }

    //A (non-building) organization
    and Organization = {
        OrganizationNumber: string
        IsActive: bool
        OrganizationType: OrganizationType
        Name: string
        Address: Address
        BankAccount: BankAccount option
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
        //Sir, Madame, etc.
        LetterPreamble: string
        MainAddress: Address
        OtherAddresses: Address list
    }
    and Gender =
        | Male
        | Female
        | Other
    and Language = {
        LanguageId: Guid
        Name: string
        //ISO 639-1 Code
        Code: string
    }
    //This is a many to many link between persons and buildings
    //In theory, the same person can be a resident multiple times for the same building (at different times)
    //A same person can also be a resident of multiple buildings
    and Resident = {
        ResidentId: Guid
        BuildingId: Guid
        Person: Person
        IsActive: bool
        MovedInDate: DateTimeOffset
        MovedOutDate: DateTimeOffset option
    }
    and ProfessionalSyndic = {
        ProfessionalSyndicId: Guid
        Person: Person
        IsActive: bool
        StartDate: DateTimeOffset
        EndDate: DateTimeOffset option
    }
    and ResidentListItem = {
        ResidentId: Guid
        BuildingId: Guid
        FirstName: string
        LastName: string
        IsActive: bool
        MovedInDate: DateTimeOffset
        MovedOutDate: DateTimeOffset option
    }
    and ProfessionalSyndicListItem = {
        ProfessionalSyndicId: Guid
        FirstName: string
        LastName: string
        IsActive: bool
        StartDate: DateTimeOffset
        EndDate: DateTimeOffset option
    }

    type InvariantError = {
        Path: string option
        Message: string
    }