module Shared.Read

open System
open Library
open MediaLibrary

type BuildingId = Guid

type GeneralMeetingPeriod = {
    FromDay: int
    FromMonth: int
    UntilDay: int
    UntilMonth: int
}

type Savable<'T> = {
    IsSaving: bool
    Payload: 'T
}

type User = 
    {
        UserId: Guid
        EmailAddress: string
        DisplayName: string
        Roles: Role list
        PreferredLanguageCode: string
        UseTwoFac: bool
    }
    member me.HasAccessToBuilding (buildingId: BuildingId) =
        me.Roles |> List.exists (
            function
            | UserRole bids -> bids |> List.contains buildingId
            | SyndicRole bIds -> bIds |> List.contains buildingId
            | ProfessionalSyndicRole (_orgId, bIds) -> bIds |> List.contains buildingId
            | SysAdminRole -> true
        )
    member me.HasAdminAccessToBuilding (buildingId: BuildingId) =
        me.Roles |> List.exists (
            function
            | UserRole _ -> false
            | SyndicRole bIds -> bIds |> List.contains buildingId
            | ProfessionalSyndicRole (_orgId, bIds) -> bIds |> List.contains buildingId
            | SysAdminRole -> true
        )
    member me.HasAccessToAdminMode () = me.Roles |> List.exists (function | SyndicRole | ProfessionalSyndicRole | SysAdminRole -> true | UserRole -> false)
    member me.IsSysAdmin () = me.Roles |> List.contains SysAdminRole
    member me.Principal () = me.EmailAddress

and Role =
    | UserRole of buildingIds: BuildingId list
    | SyndicRole of buildingIds: BuildingId list
    | ProfessionalSyndicRole of organizationId: Guid * buildingIds: BuildingId list
    //Has access to all buildings
    | SysAdminRole
    member me.BuildingIds () =
        match me with
        | UserRole bIds -> bIds
        | SyndicRole bIds -> bIds
        | ProfessionalSyndicRole (_orgId, bIds) -> bIds
        | SysAdminRole -> []

type Building = 
    {
        BuildingId: BuildingId
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
        Address = Address.Init ()
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
    static member Init () = { 
        Street = None
        ZipCode = None
        Town = None
        Country = Some "België" 
    }
    override me.ToString () =
        [
            if me.Street.IsSome then yield me.Street.Value
            if me.ZipCode.IsSome || me.Town.IsSome then yield ([me.ZipCode; me.Town] |> String.JoinOptionsWith " ")
            if me.Country.IsSome then yield me.Country.Value
        ]
        |> String.JoinWith ", "
and OtherAddress = 
    {
        Name: string
        Description: string
        Address: Address
    }
    static member Init () = {
        Name = ""
        Description = ""
        Address = Address.Init ()
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
    BuildingId: BuildingId
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
        BuildingId: BuildingId
        Owners: (LotOwner * LotOwnerRole) list
        Code: string
        LotType: LotType
        Description: string option
        //Which floor this lot is on
        Floor: int option
        //Surface in square metres, if necessary, could be calculated in square feet
        Surface: int option
    }
    member me.LegalRepresentative () =
        me.Owners
        |> List.tryPick (fun (owner, role) -> if role = LegalRepresentative then Some owner else None)
    static member Init (buildingId: BuildingId) = {
        LotId = Guid.NewGuid()
        BuildingId = buildingId
        Owners = []
        Code = ""
        LotType = LotType.Appartment
        Description = None
        Floor = None
        Surface = None
    }
and LotOwner = 
    | Owner of OwnerListItem
    | Organization of OrganizationListItem
and LotOwnerRole =
    | LegalRepresentative
    | Other
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
    BuildingId: BuildingId
    LegalRepresentative: LotOwnerListItem option
    Code: string
    LotType: LotType
    Floor: int option
    Description: string option
}
and LotOwnerListItem =
    | Owner of {| PersonId: Guid; Name: string |}
    | Organization of {| OrganizationId: Guid; Name: string |}

//A (non-building) organization
and Organization = 
    {
        OrganizationId: Guid
        BuildingId: BuildingId option
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
    static member Init (buildingId: BuildingId option): Organization = 
        let orgId = Guid.NewGuid()
        {
            OrganizationId = orgId
            BuildingId = buildingId
            OrganizationNumber = None
            VatNumber = None
            VatNumberVerifiedOn = None
            OrganizationTypes = []
            Name = ""
            Address = Address.Init ()
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
        BuildingId: BuildingId option
        Person: Person
        RoleWithinOrganization: string
    }
    static member Init (orgId: Guid) (buildingId: BuildingId option) = {
        OrganizationId = orgId
        BuildingId = buildingId
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
    BuildingId: BuildingId option
    VatNumber: string option
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
    member me.FullName () = [ me.FirstName; me.LastName ] |> String.JoinOptionsWith " "
    static member Init () = {
        PersonId = Guid.NewGuid()
        FirstName = None
        LastName = None
        LanguageCode = Some "nl-BE"
        Gender = Male
        Title = None
        MainAddress = Address.Init ()
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
        BuildingId: BuildingId
        Person: Person
        IsResident: bool
    }
    static member Init (buildingId: BuildingId) = {
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
        BuildingIds: BuildingId list
    }
    static member Init () = {
        Organization = Organization.Init None
        BuildingIds = []
    }
and OwnerListItem = 
    {
        BuildingId: BuildingId
        PersonId: Guid
        FirstName: string option
        LastName: string option
        IsResident: bool
    }
    member me.FullName () = [ me.FirstName; me.LastName ] |> String.JoinOptionsWith " "
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
    Address: Address option
}

type ContractTypeQuestionId = Guid

type ContractTypeQuestion =
    | BuildingHasElevator
    | BuildingHasCommonCentralHeating
    | BuildingHasFireAlarm
    | BuildingHasFireExtinguisher
    | BuildingHasFireHoseReel
    static member AllValues () = [|
        BuildingHasElevator
        BuildingHasCommonCentralHeating
        BuildingHasFireAlarm
        BuildingHasFireExtinguisher
        BuildingHasFireHoseReel
    |]
    static member OfString s =
        match s with
        | x when x = string BuildingHasElevator -> Some BuildingHasElevator
        | x when x = string BuildingHasCommonCentralHeating -> Some BuildingHasCommonCentralHeating
        | x when x = string BuildingHasFireAlarm -> Some BuildingHasFireAlarm
        | x when x = string BuildingHasFireExtinguisher -> Some BuildingHasFireExtinguisher
        | x when x = string BuildingHasFireHoseReel -> Some BuildingHasFireHoseReel
        | _ -> None

type PredefinedContractType =
    | ElevatorMaintenance
    | ElevatorInspection
    | CommonCentralHeatingInspection
    | FireAlarmInspection
    | FireExtinguisherInspection
    | FireHoseReelInspection
    | FireInsurance
    | LiabilityInsurance
    | CivilLiabilityForCoOwnerCouncil
    | ElectricitySupplier
    | WaterSupplier

type ContractTypeAnswer = {
    BuildingId: BuildingId
    Question: ContractTypeQuestion
    IsTrue: bool
}

type ContractType = {
    ContractTypeId: Guid
    Name: string
}

type ContractContractType =
    | PredefinedContractType of PredefinedContractType
    | OtherContractType of string
type Contract = {
    ContractId: Guid
    BuildingId: Guid
    ContractType: ContractContractType
    ContractFile: MediaFile option
    ContractOrganization: OrganizationListItem option
}

let mandatoryContractTypesFor (answer: ContractTypeAnswer) =
    if answer.IsTrue then
        match answer.Question with
        | BuildingHasElevator -> 
            [|
                PredefinedContractType.ElevatorInspection
                PredefinedContractType.ElevatorMaintenance
            |]
        | BuildingHasCommonCentralHeating -> 
            [|
                PredefinedContractType.CommonCentralHeatingInspection
            |]
        | BuildingHasFireAlarm ->
            [|
                PredefinedContractType.FireAlarmInspection
            |]
        | BuildingHasFireExtinguisher ->
            [|
                PredefinedContractType.FireExtinguisherInspection
            |]
        | BuildingHasFireHoseReel ->
            [|
                PredefinedContractType.FireHoseReelInspection
            |]
    else
        [||]

let MandatoryContractTypes = [|
    PredefinedContractType.FireInsurance
    PredefinedContractType.LiabilityInsurance
    PredefinedContractType.CivilLiabilityForCoOwnerCouncil
    PredefinedContractType.ElectricitySupplier
    PredefinedContractType.WaterSupplier
|]