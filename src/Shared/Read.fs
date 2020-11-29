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
        Validated: bool option
    }
    override me.ToString () =
        [
            if not (String.IsNullOrWhiteSpace(me.Description)) then yield me.Description
            if not (String.IsNullOrWhiteSpace(me.IBAN)) then yield me.IBAN
            if not (String.IsNullOrWhiteSpace(me.BIC)) then yield me.BIC
        ]
        |> String.joinWith " - "
    static member Init () = {
        Description = ""
        IBAN = ""
        BIC = ""
        Validated = None
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
    member me.HasAdminAccessToProfessionalSyndic (organizationId: Guid) =
        me.Roles |> List.exists (
            function
            | UserRole _ -> false
            | SyndicRole _ -> false
            | ProfessionalSyndicRole (orgId, _) -> orgId = organizationId
            | SysAdminRole -> true
        )
    member me.HasAccessToAdminMode () = me.Roles |> List.exists (function | SyndicRole _ | ProfessionalSyndicRole _ | SysAdminRole -> true | UserRole _ -> false)
    member me.IsSysAdmin () = me.Roles |> List.contains SysAdminRole
    member me.Principal () = me.EmailAddress
    static member Init () = {
        UserId = Guid.NewGuid()
        EmailAddress = ""
        DisplayName = ""
        Roles = []
        PreferredLanguageCode = "nl-BE"
        UseTwoFac = false
    }

and Role =
    | UserRole of buildingIds: BuildingId list //Regular User
    | SyndicRole of buildingIds: BuildingId list //Owner Syndic
    | ProfessionalSyndicRole of organizationId: Guid * buildingIds: BuildingId list //Pro Syndic
    | SysAdminRole //Has access to all buildings
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
        PictureId: Guid option
        SharesTotal: int
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
        PictureId = None
        SharesTotal = 0
    }
    member me.ToListItem (): BuildingListItem = {
        BuildingId = me.BuildingId
        Code = me.Code
        Name = me.Name
        Address = me.Address
        OrganizationNumber = me.OrganizationNumber
        BankAccounts = me.BankAccounts
        PictureId = me.PictureId
        SharesTotal = me.SharesTotal
    }
and Address =
    {
        Street: string option
        MailboxNumber: string option
        ZipCode: string option
        Town: string option
        Country: string option
    }
    static member Init () = { 
        Street = None
        MailboxNumber = None
        ZipCode = None
        Town = None
        Country = Some "België" 
    }
    static member Copy (otherAddress: Address) = {
        Street = otherAddress.Street
        MailboxNumber = otherAddress.MailboxNumber
        ZipCode = otherAddress.ZipCode
        Town = otherAddress.Town
        Country = otherAddress.Country
    }
    override me.ToString () =
        let houseNumber =
            [
                if me.Street.IsSome then yield me.Street.Value
                if me.MailboxNumber.IsSome then yield (sprintf "bus %s" me.MailboxNumber.Value)
            ]
            |> String.joinWith " "

        [
            if (not (String.IsNullOrWhiteSpace houseNumber)) then yield houseNumber
            if me.ZipCode.IsSome || me.Town.IsSome then yield ([me.ZipCode; me.Town] |> String.joinOptionsWith " ")
            if me.Country.IsSome then yield me.Country.Value
        ]
        |> String.joinWith ", "
and OtherAddress = 
    {
        Description: string
        Address: Address
    }
    static member Init () = {
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
    PictureId: Guid option
    SharesTotal: int
}
and Lot = 
    {
        LotId: Guid
        BuildingId: BuildingId
        Owners: LotOwner list
        Code: string
        LotType: LotType
        Description: string option
        //Which floor this lot is on
        Floor: int option
        //Nl: aandeel / quotiteit
        Share: int option
    }
    member me.LegalRepresentative () =
        me.Owners
        |> List.tryFind (fun owner -> owner.StartDate < DateTimeOffset.Now && (owner.EndDate.IsNone || owner.EndDate.Value > DateTimeOffset.Now))
    static member Init (buildingId: BuildingId) = {
        LotId = Guid.NewGuid()
        BuildingId = buildingId
        Owners = []
        Code = ""
        LotType = LotType.Appartment
        Description = None
        Floor = None
        Share = None
    }
and LotOwner = 
    {
        LotId: Guid
        LotOwnerId: Guid
        LotOwnerType: LotOwnerType
        StartDate: DateTimeOffset
        EndDate: DateTimeOffset option
        Contacts: LotOwnerContact list
    }
    member me.FullName () =
        match me.LotOwnerType with
        | LotOwnerType.Organization org -> org.Name
        | LotOwnerType.Owner owner -> owner.FullName ()
and LotOwnerContact =
    | Owner of OwnerListItem
    | NonOwner of Person
    member me.PersonId =
        match me with
        | Owner o -> o.PersonId
        | NonOwner p -> p.PersonId
and LotOwnerType =
    | Owner of OwnerListItem
    | Organization of OrganizationListItem
and LotType =
    | Appartment
    | Studio
    | ParkingSpace
    | CommercialProperty
    | Garage
    | Storage
    | Other
    static member AllValues () =
        [ Appartment; Studio; ParkingSpace; CommercialProperty; Garage; Storage; Other ]
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
    Share: int option
}
and LotOwnerListItem =
    | Owner of {| PersonId: Guid; Name: string |}
    | Organization of {| OrganizationId: Guid; Name: string |}
    member me.Name =
        match me with
        | Owner owner -> owner.Name
        | Organization org -> org.Name

//A (non-building) organization
and Organization = 
    {
        OrganizationId: Guid
        BuildingId: BuildingId option
        UsesVatNumber: bool
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
    static member Init (buildingId: BuildingId option, usesVatNumber: bool): Organization = 
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
            UsesVatNumber = usesVatNumber
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
    member me.FullName () = [ me.FirstName; me.LastName ] |> String.joinOptionsWith " "
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
    member me.ToListItem (): OwnerListItem = {
        BuildingId = me.BuildingId
        PersonId = me.Person.PersonId
        FirstName = me.Person.FirstName
        LastName = me.Person.LastName
        IsResident = me.IsResident
        BankAccounts = me.Person.BankAccounts
    }

and ProfessionalSyndic = 
    {
        Organization: Organization
        BuildingIds: BuildingId list
    }
    static member Init () = {
        Organization = Organization.Init (None, false)
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
    member me.FullName () = [ me.FirstName; me.LastName ] |> String.joinOptionsWith " "

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
|]

let OtherPredefinedContractTypes = [|
    PredefinedContractType.ElectricitySupplier
    PredefinedContractType.WaterSupplier
|]

type DistributionKey = {
    DistributionKeyId: Guid
    BuildingId: Guid option
    Name: string
    DistributionType: DistributionType
    LotsOrLotTypes: LotsOrLotTypes
    IncludeGroundFloor: bool
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

type FinancialYear = 
    {
        FinancialYearId: Guid
        BuildingId: Guid
        Code: string
        StartDate: DateTime
        EndDate: DateTime
        IsClosed: bool
    }
    static member Init (buildingId: Guid) = {
        FinancialYearId = Guid.NewGuid()
        BuildingId = buildingId
        Code = ""
        StartDate = DateTime.Today
        EndDate = DateTime.Today.AddYears(1)
        IsClosed = false
    }

type FinancialCategory = 
    {
        FinancialCategoryId: Guid
        BuildingId: Guid option
        Code: string
        Description: string
    }
    static member Init (buildingId: Guid) = {
        FinancialCategoryId = Guid.NewGuid()
        BuildingId = Some buildingId
        Code = ""
        Description = ""
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
        Cost: decimal
        DistributionKeyName: string
        OrganizationId: Guid
        OrganizationName: string
        CategoryCode: string //Boekhoudkundige rekening
        CategoryDescription: string
        InvoiceDate: DateTimeOffset
        DueDate: DateTimeOffset
        //HasBeenPaid: bool TODO
    }
    member me.LocalInvoiceNumber = Invoice.calculateLocalInvoiceNumber (me.FinancialYearCode, me.InvoiceNumber)

type InvoiceFilterPeriod =
    | FinancialYear of financialYearId: Guid
    | Month of month: int * year: int
    | Year of year: int

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
        Cost: Decimal
        VatRate: int
        FinancialCategory: FinancialCategory
        BookingDate: DateTime //Date when booked
        DistributionKey: DistributionKeyListItem
        Organization: OrganizationListItem
        OrganizationBankAccount: BankAccount
        OrganizationInvoiceNumber: string option //Number @ supplier
        InvoiceDate: DateTimeOffset //Date on the invoice
        DueDate: DateTimeOffset //Due date of the invoice
        //PaymentIds: Guid list
        MediaFiles: MediaFile list
    }
    member me.LocalInvoiceNumber = Invoice.calculateLocalInvoiceNumber (me.FinancialYear.Code, me.InvoiceNumber)
