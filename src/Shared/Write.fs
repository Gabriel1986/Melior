module Shared.Write

open System
open Read
open ConstrainedTypes
open Library
open Trial.Control
open Trial

type ValidatedAddress = 
    {
        Street: String255 option
        ZipCode: String16 option
        Town: String64 option
        Country: String64 option
    }
    static member BasicValidate (basePath: string) (address: Address) = 
        let onBasePath subPath = sprintf "%s.%s" basePath subPath
        trial {
            from street in validateOptional (String255.Of (onBasePath (nameof address.Street))) address.Street
            also zipCode in validateOptional (String16.Of (onBasePath (nameof address.ZipCode))) address.ZipCode
            also town in validateOptional (String64.Of (onBasePath (nameof address.Town))) address.Town
            also country in validateOptional (String64.Of (onBasePath (nameof address.Country))) address.Country
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
    static member BasicValidate (basePath: string) (otherAddress: OtherAddress) =
        let onBasePath subPath = sprintf "%s.%s" basePath subPath
        trial {
            from name in String255.Of (onBasePath (nameof otherAddress.Name)) otherAddress.Name
            also address in ValidatedAddress.BasicValidate (onBasePath (nameof otherAddress.Address)) otherAddress.Address
            yield {
                Name = name
                Description = otherAddress.Description
                Address = address
            }
        }

type ValidatedBankAccount = 
    {
        Description: String255 option
        IBAN: String64 option
        BIC: String16 option
    }
    static member BasicValidate (basePath: string) (bankAccount: BankAccount) =
        let onBasePath subPath = sprintf "%s.%s" basePath subPath
        trial {
            from description in String255.OfOptional (onBasePath (nameof bankAccount.Description)) bankAccount.Description
            also iban in String64.OfOptional (onBasePath (nameof bankAccount.IBAN)) bankAccount.IBAN
            also bic in String16.OfOptional (onBasePath (nameof bankAccount.BIC)) bankAccount.BIC
            yield {
                Description = description
                IBAN = iban
                BIC = bic
            }
        }


type ValidatedContactMethod = 
    {
        ContactMethodType: ContactMethodType
        Value: String255
        Description: string
    }
    static member BasicValidate (basePath: string) (contactMethod: ContactMethod) =
        let onBasePath subPath = sprintf "%s.%s" basePath subPath
        trial {
            from value in String255.Of (onBasePath (nameof contactMethod.Value)) contactMethod.Value
            yield {
                ContactMethodType = contactMethod.ContactMethodType
                Value = value
                Description = contactMethod.Description
            }
        }

let private validateContactAddress (path: string) (contactAddress: ContactAddress) =
    match contactAddress with
    | MainAddress -> Trial.Pass None
    | ContactAddress c -> ValidatedAddress.BasicValidate path c |> Trial.map Some

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
        BankAccounts: ValidatedBankAccount list
    }
    static member BasicValidate (basePath: string) (person: Person) =
        let onBasePath subPath = sprintf "%s.%s" basePath subPath
        trial {
           from firstName in validateOptional (String255.Of (nameof person.FirstName |> onBasePath)) person.FirstName
           also lastName in validateOptional (String255.Of (nameof person.LastName |> onBasePath)) person.LastName
           also languageCode in validateOptional (String16.Of (nameof person.LanguageCode |> onBasePath)) person.LanguageCode
           also title in validateOptional (String32.Of (nameof person.Title |> onBasePath)) person.Title
           also mainTelephoneNumber in validateOptional (String32.Of (nameof person.MainTelephoneNumber |> onBasePath)) person.MainTelephoneNumber
           also mainTelephoneNumberComment in validateOptional (String255.Of (nameof person.MainTelephoneNumberComment |> onBasePath)) person.MainTelephoneNumberComment
           also mainEmailAddress in validateOptional (String255.Of (nameof person.MainEmailAddress |> onBasePath)) person.MainEmailAddress
           also mainEmailAddressComment in validateOptional (String255.Of (nameof person.MainEmailAddressComment |> onBasePath))  person.MainEmailAddressComment
           also mainAddress in ValidatedAddress.BasicValidate (nameof person.MainAddress |> onBasePath) person.MainAddress
           also contactAddress in validateContactAddress (nameof person.ContactAddress |> onBasePath) person.ContactAddress
           also otherContactMethods in person.OtherContactMethods |> List.mapi (fun index c -> ValidatedContactMethod.BasicValidate (sprintf "%s.[%i]" (nameof person.OtherContactMethods) index |> onBasePath) c) |> Trial.sequence
           also otherAddresses in person.OtherAddresses |> List.mapi (fun index a -> ValidatedOtherAddress.BasicValidate (sprintf "%s.[%i]" (nameof person.OtherAddresses) index |> onBasePath) a) |> Trial.sequence
           also bankAccounts in person.BankAccounts |> List.mapi (fun index b -> ValidatedBankAccount.BasicValidate (sprintf "%s.[%i]" (nameof person.BankAccounts) index |> onBasePath) b) |> Trial.sequence
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
               BankAccounts = bankAccounts |> List.ofSeq
           }
        }
    static member Validate (person: Person) =
        ValidatedPerson.BasicValidate "" person
        |> Trial.toResult

type ValidatedOwner = 
    {
        BuildingId: Guid
        Person: ValidatedPerson
        IsResident: bool
    }
    static member Validate (owner: Owner): Result<ValidatedOwner, (string * string) list> =
        trial {
            from person in ValidatedPerson.BasicValidate (nameof owner.Person) owner.Person
            yield {
                BuildingId = owner.BuildingId
                Person = person
                IsResident = owner.IsResident
            }
        }
        |> Trial.toResult

type SyndicInput =
    | OwnerId of ownerId: Guid
    | ProfessionalSyndicId of proSyndicId: Guid
    | Other of Person

type ConciergeInput =
    | OwnerId of ownerId: Guid
    | NonOwner of Person

let mapSyndicReadToWrite =
    function
    | Syndic.ProfessionalSyndic prof -> 
        SyndicInput.ProfessionalSyndicId prof.Organization.OrganizationId
    | Syndic.Owner owner ->
        SyndicInput.OwnerId owner.Person.PersonId
    | Syndic.Other person ->
        SyndicInput.Other person

let mapConciergeReadToWrite =
    function
    | Read.Concierge.Owner owner ->
        OwnerId owner.Person.PersonId
    | Read.Concierge.NonOwner person ->
        NonOwner person

type ValidatedBuilding = 
    {
        BuildingId: Guid
        Code: String16
        Name: String255
        Address: ValidatedAddress
        OrganizationNumber: OrganizationNumber option
        Remarks: string option
        GeneralMeetingPeriod: GeneralMeetingPeriod option
        YearOfConstruction: PositiveInt option
        YearOfDelivery: PositiveInt option
        BankAccounts: ValidatedBankAccount list
    }
    static member Validate (building: Building): Result<ValidatedBuilding, (string * string) list> =
        trial {
            from code in String16.Of (nameof building.Code) building.Code
            also name in String255.Of (nameof building.Name) building.Name
            also yearOfConstruction in validateOptional (PositiveInt.Of (nameof building.YearOfConstruction)) building.YearOfConstruction
            also yearOfDelivery in validateOptional (PositiveInt.Of (nameof building.YearOfDelivery))  building.YearOfDelivery
            also address in ValidatedAddress.BasicValidate (nameof building.Address) building.Address
            also orgNr in validateOptional (OrganizationNumber.OfString (nameof building.OrganizationNumber)) building.OrganizationNumber
            also bankAccounts in building.BankAccounts |> List.mapi (fun index b -> ValidatedBankAccount.BasicValidate (sprintf "%s.[%i]" (nameof building.BankAccounts) index) b) |> Trial.sequence
            yield {
                BuildingId = building.BuildingId
                Code = code
                Name = name
                Address = address
                OrganizationNumber = orgNr
                Remarks = building.Remarks
                GeneralMeetingPeriod = building.GeneralMeetingPeriod
                YearOfConstruction = yearOfConstruction
                YearOfDelivery = yearOfDelivery
                BankAccounts = bankAccounts |> List.ofSeq
            }
        }
        |> Trial.toResult


type LotOwnerId =
    | OwnerId of Guid
    | OrganizationId of Guid

let private mapLotOwner =
    function
    | LotOwner.Owner owner -> OwnerId owner.PersonId
    | LotOwner.Organization organization -> OrganizationId organization.OrganizationId

type ValidatedLot = 
    {
        LotId: Guid
        BuildingId: Guid
        Owners: (LotOwnerId * LotOwnerRole) list
        Code: String16
        LotType: LotType
        Description: string option
        Floor: int option //Floor can be negative, it's only constrained in range
        Surface: PositiveInt option
    }
    static member Validate (lot: Lot) = 
        trial {
            from code in String16.Of (nameof lot.Code) lot.Code
            also surface in validateOptional (PositiveInt.Of (nameof lot.Surface)) lot.Surface
            yield {
                LotId = lot.LotId
                BuildingId = lot.BuildingId
                Owners = (lot.Owners |> List.map (fun (owner, role) -> mapLotOwner owner, role))
                Code = code
                LotType = lot.LotType
                Description = lot.Description
                Floor = lot.Floor
                Surface = surface
            }
        }
        |> Trial.toResult

type ValidatedContactPerson = 
    {
        OrganizationId: Guid
        BuildingId: Guid option
        Person: ValidatedPerson
        RoleWithinOrganization: String32
    }
    static member Validate (cp: ContactPerson) =
        trial {
            from person in ValidatedPerson.BasicValidate (nameof cp.Person) cp.Person
            also role in String32.Of (nameof cp.RoleWithinOrganization) cp.RoleWithinOrganization
            yield {
                OrganizationId = cp.OrganizationId
                BuildingId = cp.BuildingId
                Person = person
                RoleWithinOrganization = role
            }
        }
        |> Trial.toResult

type ValidatedOrganization =
    {
        OrganizationId: Guid
        BuildingId: Guid option //Pro syndics do NOT have a buildingId
        OrganizationNumber: OrganizationNumber option
        VatNumber: VatNumber option
        VatNumberVerifiedOn: DateTime option
        OrganizationTypeIds: Guid list
        Name: String255
        Address: ValidatedAddress
        MainEmailAddress: String255 option
        MainEmailAddressComment: String255 option
        MainTelephoneNumber: String32 option
        MainTelephoneNumberComment: String255 option
        OtherContactMethods: ValidatedContactMethod list
        BankAccounts: ValidatedBankAccount list
    }
    static member BasicValidate (basePath: string) (organization: Organization) =
        let onBasePath (s: string) = sprintf "%s.%s" basePath s
        trial {
            from name in String255.Of (nameof organization.Name |> onBasePath) organization.Name
            also address in ValidatedAddress.BasicValidate (nameof organization.Address |> onBasePath) organization.Address
            also organizationNumber in validateOptional (OrganizationNumber.OfString (nameof organization.OrganizationNumber |> onBasePath)) organization.OrganizationNumber
            also vatNumber in validateOptional (VatNumber.OfString (nameof organization.VatNumber |> onBasePath)) organization.VatNumber
            also mainTelephoneNumber in validateOptional (String32.Of (nameof organization.MainTelephoneNumber |> onBasePath)) organization.MainTelephoneNumber
            also mainTelephoneNumberComment in validateOptional (String255.Of (nameof organization.MainTelephoneNumberComment |> onBasePath)) organization.MainTelephoneNumberComment
            also mainEmailAddress in validateOptional (String255.Of (nameof organization.MainEmailAddress |> onBasePath)) organization.MainEmailAddress
            also mainEmailAddressComment in validateOptional (String255.Of (nameof organization.MainEmailAddressComment |> onBasePath))  organization.MainEmailAddressComment
            also otherContactMethods in organization.OtherContactMethods |> List.mapi (fun index c -> ValidatedContactMethod.BasicValidate (sprintf "%s.[%i]" (nameof organization.OtherContactMethods) index |> onBasePath) c) |> Trial.sequence
            also bankAccounts in organization.BankAccounts |> List.mapi (fun index b -> ValidatedBankAccount.BasicValidate (sprintf "%s.[%i]" (nameof organization.BankAccounts) index |> onBasePath) b) |> Trial.sequence
            yield {
                OrganizationId = organization.OrganizationId
                BuildingId = organization.BuildingId
                OrganizationNumber = organizationNumber
                VatNumber = vatNumber
                VatNumberVerifiedOn = organization.VatNumberVerifiedOn
                OrganizationTypeIds = organization.OrganizationTypes |> List.map (fun ot -> ot.OrganizationTypeId)
                Name = name
                Address = address
                MainTelephoneNumber = mainTelephoneNumber
                MainTelephoneNumberComment = mainTelephoneNumberComment
                MainEmailAddress = mainEmailAddress
                MainEmailAddressComment = mainEmailAddressComment
                OtherContactMethods = otherContactMethods |> List.ofSeq
                BankAccounts = bankAccounts |> List.ofSeq
            }
        }
    static member Validate (organization: Organization) =
        ValidatedOrganization.BasicValidate "" organization
        |> Trial.toResult

type ValidatedProfessionalSyndic = 
    {
        Organization: ValidatedOrganization
    }
    static member Validate (proSyndic: ProfessionalSyndic) =
        trial {
            from organization in ValidatedOrganization.BasicValidate (nameof proSyndic.Organization) proSyndic.Organization
            yield {
                Organization = organization
            }
        }
        |> Trial.toResult

type ValidatedOrganizationType =
    {
        OrganizationTypeId: Guid
        Name: String255
    }
    static member Validate (orgType: OrganizationType) =
        trial {
            from name in String255.Of (nameof orgType.Name) orgType.Name
            yield {
                OrganizationTypeId = orgType.OrganizationTypeId
                Name = name
            }
        }
        |> Trial.toResult

type ValidatedContract =
    {
        ContractId: Guid
        BuildingId: Guid
        ContractType: ValidatedContractContractType
        ContractFileId: Guid option
        ContractOrganizationId: Guid option
    }
    static member Validate (contract: Contract) =
        let validatedContractType =
            match contract.ContractType with
            | PredefinedContractType predefined -> 
                Trial.Pass (ValidatedPredefinedContractType predefined)
            | OtherContractType other -> 
                String255.Of (nameof contract.ContractType) other 
                |> Trial.map ValidatedOtherContractType

        trial {
            from contractType in validatedContractType
            yield {
                ContractId = contract.ContractId
                BuildingId = contract.BuildingId
                ContractType = contractType
                ContractFileId = contract.ContractFile |> Option.map (fun f -> f.FileId)
                ContractOrganizationId = contract.ContractOrganization |> Option.map (fun o -> o.OrganizationId) 
            }
        }
        |> Trial.toResult

and ValidatedContractContractType =
    | ValidatedPredefinedContractType of PredefinedContractType
    | ValidatedOtherContractType of String255

type ValidatedDistributionKey = 
    {
        DistributionKeyId: Guid
        BuildingId: Guid option
        Name: String255
        DistributionType: DistributionType
        LotsOrLotTypes: LotsOrLotTypes
    }
    static member Validate (distributionKey: DistributionKey) =
        trial {
            from name in (String255.Of (nameof distributionKey.Name) distributionKey.Name)
            yield {
                DistributionKeyId = distributionKey.DistributionKeyId
                BuildingId = distributionKey.BuildingId
                Name = name
                DistributionType = distributionKey.DistributionType
                LotsOrLotTypes = distributionKey.LotsOrLotTypes
            }
        }
        |> Trial.toResult

type ValidatedUser =
    {
        UserId: Guid
        EmailAddress: String255
        DisplayName: String255
        PreferredLanguageCode: String16
        Roles: Role list
    }
    static member Validate (user: User) =
        trial {
            from emailAddress in String255.Of (nameof user.EmailAddress) user.EmailAddress
            also displayName in String255.Of (nameof user.DisplayName) user.DisplayName
            also languageCode in String16.Of (nameof user.PreferredLanguageCode) user.PreferredLanguageCode
            yield {
                UserId = user.UserId
                EmailAddress = emailAddress
                DisplayName = displayName
                PreferredLanguageCode = languageCode
                Roles = user.Roles
            }
        }
        |> Trial.toResult

type SimpleUserRole = {
    UserId: Guid
    Role: SimpleRole
    BuildingId: Guid option
}
and SimpleRole =
    | User
    | Syndic

type UserProfessionalSyndicLink = {
    UserId: Guid
    ProfessionalSyndicId: Guid
}

type ProfessionalSyndicBuilding = {
    ProfessionalSyndic: Guid
    BuildingId: Guid
}

type ValidatedInvoice = 
    {
        InvoiceId: Guid
        BuildingId: Guid
        FinancialYear: FinancialYear
        Description: string option
        Cost: float
        VatRate: PositiveFloat
        CategoryCode: String16
        CategoryDescription: String64
        FromBankAccount: ValidatedBankAccount
        ToBankAccount: ValidatedBankAccount
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
    }
    static member Validate (invoice: Invoice) =
        trial {
            from vatRate in validatePositiveFloat (nameof invoice.VatRate) invoice.VatRate
            also categoryCode in String16.Of (nameof invoice.CategoryCode) invoice.CategoryCode
            also categoryDescription in String64.Of (nameof invoice.CategoryDescription) invoice.CategoryDescription
            also fromBankAccount in ValidatedBankAccount.BasicValidate (nameof invoice.FromBankAccount) invoice.FromBankAccount
            also toBankAccount in ValidatedBankAccount.BasicValidate (nameof invoice.ToBankAccount) invoice.ToBankAccount
            yield {
                InvoiceId = invoice.InvoiceId
                BuildingId = invoice.BuildingId
                FinancialYear = invoice.FinancialYear
                Description = invoice.Description
                Cost = invoice.Cost
                VatRate = vatRate
                CategoryCode = categoryCode
                CategoryDescription = categoryDescription
                FromBankAccount = fromBankAccount
                ToBankAccount = toBankAccount
                BookingDate = invoice.BookingDate
                DistributionKey = invoice.DistributionKey
                InvoiceDate = invoice.InvoiceDate
                DueDate = invoice.DueDate
                ExternalInvoiceNumber = invoice.ExternalInvoiceNumber
                OrganizationId = invoice.OrganizationId
                OrganizationName = invoice.OrganizationName
                OrganizationNumber = invoice.OrganizationNumber
                OrganizationVatNumber = invoice.OrganizationVatNumber
                PaymentIds = invoice.PaymentIds
            }
        }        
        |> Trial.toResult