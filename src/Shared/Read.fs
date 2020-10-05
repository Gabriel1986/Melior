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

type BankAccount = 
    {
        Description: string
        IBAN: string
        BIC: string
    }
    override me.ToString () =
        [
            if not (String.IsNullOrWhiteSpace(me.Description)) then yield me.Description
            if not (String.IsNullOrWhiteSpace(me.IBAN)) then yield me.IBAN
            if not (String.IsNullOrWhiteSpace(me.BIC)) then yield me.BIC
        ]
        |> String.JoinWith " - "
    static member Init () = {
        Description = ""
        IBAN = ""
        BIC = ""
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
    member me.HasAccessToAdminMode () = me.Roles |> List.exists (function | SyndicRole _ | ProfessionalSyndicRole _ | SysAdminRole -> true | UserRole _ -> false)
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

and RoleKind =
    | UserRoleKind
    | SyndicRoleKind
    | ProfessionalSyndicRoleKind
    | SysAdminRoleKind

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
        BankAccounts: BankAccount list
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
        BankAccounts = []
    }
    member me.ToListItem (): BuildingListItem = {
        BuildingId = me.BuildingId
        Code = me.Code
        Name = me.Name
        Address = me.Address
        OrganizationNumber = me.OrganizationNumber
        BankAccounts = me.BankAccounts
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
    static member Copy (otherAddress: Address) = {
        Street = otherAddress.Street
        ZipCode = otherAddress.ZipCode
        Town = otherAddress.Town
        Country = otherAddress.Country
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
    override me.ToString () =
        match me with
        | PhoneNumber -> "Tel. nr."
        | EmailAddress -> "Email"
        | WebSite -> "Website"
        | Other -> "Andere"
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
    BankAccounts: BankAccount list
}

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
        BankAccounts: BankAccount list
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
            BankAccounts = []
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
    BankAccounts: BankAccount list
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
        BankAccounts: BankAccount list
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
        BankAccounts = []
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
//and Tenant = {
//    Person: Person
//    MoveInDate: DateTimeOffset
//    MoveOutDate: DateTimeOffset option
//}
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
        BankAccounts: BankAccount list
        IsResident: bool
    }
    member me.FullName () = [ me.FirstName; me.LastName ] |> String.JoinOptionsWith " "
//and TenantListItem = {
//    PersonId: Guid
//    FirstName: string
//    LastName: string
//    MoveInDate: DateTimeOffset
//    MoveOutDate: DateTimeOffset option
//}
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

//TODO: place the distribution keys in the DB
//type PredefinedDistributionKey =
//    | ``Total according to shares``
//    | ``Appartments according to shares``
//    | ``Appartments without ground floor according to shares``
//    | ``Garages and parking spaces according to shares``
//    | ``Storage and basements according to shares``
//    | ``Total according to equal parts``
//    | ``Appartments according to equal parts``
//    | ``Appartments without ground floor according to equal parts``
//    | ``Garages and parking spaces according to equal parts``
//    | ``Storage and basements according to equal parts``
//    override me.ToString () =
//        match me with
//        | ``Total according to shares`` -> "Totaal – volgens aandelen"
//        | ``Appartments according to shares`` -> "Appartementen - volgens aandelen"
//        | ``Appartments without ground floor according to shares`` -> "Appartementen zonder gelijkvloer - volgens aandelen"
//        | ``Garages and parking spaces according to shares`` -> "Staanplaatsen / garages - volgens aandelen"
//        | ``Storage and basements according to shares`` -> "Bergingen / kelders - volgens aandelen"
//        | ``Total according to equal parts`` -> "Totaal – volgens gelijke delen"
//        | ``Appartments according to equal parts`` -> "Appartementen - volgens gelijke delen"
//        | ``Appartments without ground floor according to equal parts`` -> "Appartementen zonder gelijkvloer - volgens gelijke delen"
//        | ``Garages and parking spaces according to equal parts`` -> "Staanplaatsen / garages - volgens gelijke delen"
//        | ``Storage and basements according to equal parts`` -> "Bergingen / kelders - volgens gelijke delen"

type DistributionKey = {
    DistributionKeyId: Guid
    BuildingId: Guid option
    Name: string
    DistributionType: DistributionType
    LotsOrLotTypes: LotsOrLotTypes
}
and DistributionType =
    | EqualParts
    | Shares
    override me.ToString () =
        match me with
        | EqualParts -> "Volgens gelijke delen"
        | Shares -> "Volgens aandelen"
and LotsOrLotTypes =
    | Lots of lotIds: Guid list
    | LotTypes of lotType: LotType list

type DistributionKeyListItem = {
    DistributionKeyId: Guid
    BuildingId: Guid option
    Name: string
    DistributionType: DistributionType
}

type FinancialYear = {
    FinancialYearId: Guid
    Code: string
    StartDate: DateTimeOffset
    EndDate: DateTimeOffset
    IsClosed: bool
}

type FinancialCategory = {
    FinancialCategoryId: Guid
    Code: string
    Description: string
}

module Invoice =
    let calculateLocalInvoiceNumber (financialYearCode: string, invoiceNumber: int) =
        sprintf "%s/%06i" financialYearCode invoiceNumber

type InvoiceListItem = 
    {
        InvoiceId: Guid
        BuildingId: BuildingId
        FinancialYearCode: string
        FinancialYearIsClosed: bool
        InvoiceNumber: int
        Cost: float
        VatRate: float
        DistributionKeyName: string
        OrganizationName: string
        CategoryCode: string //Rubriek
        DueDate: DateTime
        HasBeenPaid: bool
    }
    member me.LocalInvoiceNumber = Invoice.calculateLocalInvoiceNumber (me.FinancialYearCode, me.InvoiceNumber)

type InvoiceFilterPeriod =
    | FinancialYear of financialYearId: Guid
    | Year of int
    | Quarter of int
    | Month of int

type InvoiceFilter = {
    BuildingId: Guid
    Period: InvoiceFilterPeriod
}

type Invoice = 
    {
        InvoiceId: Guid
        BuildingId: Guid
        FinancialYear: FinancialYear
        InvoiceNumber: int
        Description: string option
        Cost: float
        VatRate: float
        CategoryCode: string
        CategoryDescription: string
        FromBankAccount: BankAccount
        ToBankAccount: BankAccount
        BookingDate: DateTime //Date when booked
        DistributionKey: DistributionKeyListItem
        OrganizationId: Guid
        OrganizationName: string
        OrganizationNumber: string option
        OrganizationVatNumber: string option
        ExternalInvoiceNumber: string option //Number @ supplier
        InvoiceDate: DateTime //Date on the invoice
        DueDate: DateTime //Due date of the invoice
        PaymentIds: Guid list
        MediaFiles: MediaFile list
    }
    member me.LocalInvoiceNumber = Invoice.calculateLocalInvoiceNumber (me.FinancialYear.Code, me.InvoiceNumber)
