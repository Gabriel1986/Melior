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
        MailboxNumber: String16 option
        ZipCode: String16 option
        Town: String64 option
        Country: String64 option
    }
    static member BasicValidate (basePath: string) (address: Address) = 
        let onBasePath s = if String.IsNullOrWhiteSpace basePath then s else sprintf "%s.%s" basePath s
        trial {
            from street in validateOptional (String255.Of (onBasePath (nameof address.Street))) address.Street
            also mailboxNumber in validateOptional (String16.Of (onBasePath (nameof address.MailboxNumber))) address.MailboxNumber
            also zipCode in validateOptional (String16.Of (onBasePath (nameof address.ZipCode))) address.ZipCode
            also town in validateOptional (String64.Of (onBasePath (nameof address.Town))) address.Town
            also country in validateOptional (String64.Of (onBasePath (nameof address.Country))) address.Country
            yield {
                Street = street
                MailboxNumber = mailboxNumber
                ZipCode = zipCode
                Town = town
                Country = country
            }
        }

type ValidatedOtherAddress = 
    {
        Description: string
        Address: ValidatedAddress
    }
    static member BasicValidate (basePath: string) (otherAddress: OtherAddress) =
        let onBasePath s = if String.IsNullOrWhiteSpace basePath then s else sprintf "%s.%s" basePath s
        trial {
            from address in ValidatedAddress.BasicValidate (onBasePath (nameof otherAddress.Address)) otherAddress.Address
            yield {
                Description = otherAddress.Description
                Address = address
            }
        }

type ValidatedBankAccount = 
    {
        Description: String255 option
        IBAN: IBAN option
        BIC: String16 option
        Validated: bool option
    }
    static member BasicValidate (basePath: string) (bankAccount: BankAccount) =
        let onBasePath s = if String.IsNullOrWhiteSpace basePath then s else sprintf "%s.%s" basePath s
        trial {
            from description in String255.OfOptional (onBasePath (nameof bankAccount.Description)) bankAccount.Description
            also iban in IBAN.OfOptional (onBasePath (nameof bankAccount.IBAN)) bankAccount.IBAN
            also bic in String16.OfOptional (onBasePath (nameof bankAccount.BIC)) bankAccount.BIC
            yield {
                Description = description
                IBAN = iban
                BIC = bic
                Validated = bankAccount.Validated
            }
        }


type ValidatedContactMethod = 
    {
        ContactMethodType: ContactMethodType
        Value: String255
        Description: string
    }
    static member BasicValidate (basePath: string) (contactMethod: ContactMethod) =
        let onBasePath s = if String.IsNullOrWhiteSpace basePath then s else sprintf "%s.%s" basePath s
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
        let onBasePath s = if String.IsNullOrWhiteSpace basePath then s else sprintf "%s.%s" basePath s
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
        PictureId: Guid option
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
                PictureId = building.PictureId
            }
        }
        |> Trial.toResult


type LotOwnerTypeId =
    | OwnerId of Guid
    | OrganizationId of Guid

let private mapLotOwnerTypeToId =
    function
    | LotOwnerType.Owner owner -> OwnerId owner.PersonId
    | LotOwnerType.Organization organization -> OrganizationId organization.OrganizationId

type ValidatedLotOwner = 
    {
        LotId: Guid
        LotOwnerId: Guid
        LotOwnerTypeId: LotOwnerTypeId
        LotOwnerRole: LotOwnerRole
        StartDate: DateTimeOffset
        EndDate: DateTimeOffset option
    }
    static member BasicValidate (basePath: string) (lotOwner: LotOwner): Trial<ValidatedLotOwner, string * string> =
        let onBasePath (s: string) = if String.IsNullOrWhiteSpace basePath then s else sprintf "%s.%s" basePath s
        let validateStartDate (path: string) (startDate: DateTimeOffset, endDate: DateTimeOffset option) =
            match endDate with
            | Some endDate when endDate < startDate -> Trial.ofError (path, "De begindatum moet vóór de einddatum vallen")
            | _ -> Trial.Pass startDate

        trial {
            from startDate in validateStartDate (nameof lotOwner.StartDate |> onBasePath) (lotOwner.StartDate, lotOwner.EndDate)
            yield {
                LotId = lotOwner.LotId
                LotOwnerId = lotOwner.LotOwnerId
                LotOwnerTypeId = mapLotOwnerTypeToId lotOwner.LotOwnerType
                LotOwnerRole = lotOwner.LotOwnerRole
                StartDate = startDate
                EndDate = lotOwner.EndDate
            }
        }
    static member Validate (lotOwner: LotOwner): Result<ValidatedLotOwner, (string * string) list> =
        ValidatedLotOwner.BasicValidate "" lotOwner
        |> Trial.toResult

type ValidatedLot = 
    {
        LotId: Guid
        BuildingId: Guid
        Owners: ValidatedLotOwner list
        Code: String16
        LotType: LotType
        Description: string option
        Floor: int option //Floor can be negative, it's only constrained in range
        Share: PositiveInt option
    }
    static member Validate (lot: Lot) = 
        let validateLotOwners (path: string) (owners: LotOwner list) =
            owners 
            |> List.mapi (fun index owner -> ValidatedLotOwner.BasicValidate (sprintf "%s.[%i]" path index) owner) 
            |> Trial.sequence

        trial {
            from code in String16.Of (nameof lot.Code) lot.Code
            also share in validateOptional (PositiveInt.Of (nameof lot.Share)) lot.Share
            also owners in validateLotOwners (nameof lot.Owners) lot.Owners
            yield {
                LotId = lot.LotId
                BuildingId = lot.BuildingId
                Owners = owners |> List.ofSeq
                Code = code
                LotType = lot.LotType
                Description = lot.Description
                Floor = lot.Floor
                Share = share
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
        UsesVatNumber: bool
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
        let onBasePath (s: string) = if String.IsNullOrWhiteSpace basePath then s else sprintf "%s.%s" basePath s
        trial {
            from name in String255.Of (nameof organization.Name |> onBasePath) organization.Name
            also address in ValidatedAddress.BasicValidate (nameof organization.Address |> onBasePath) organization.Address
            also organizationNumber in validateOptional (OrganizationNumber.OfString (nameof organization.OrganizationNumber |> onBasePath)) (if organization.UsesVatNumber then None else organization.OrganizationNumber)
            also vatNumber in validateOptional (VatNumber.OfString (nameof organization.VatNumber |> onBasePath)) (if organization.UsesVatNumber then organization.VatNumber else None)
            also vatNumberVerifiedOn in (if organization.UsesVatNumber then Trial.Pass organization.VatNumberVerifiedOn else Trial.Pass None)
            also mainTelephoneNumber in validateOptional (String32.Of (nameof organization.MainTelephoneNumber |> onBasePath)) organization.MainTelephoneNumber
            also mainTelephoneNumberComment in validateOptional (String255.Of (nameof organization.MainTelephoneNumberComment |> onBasePath)) organization.MainTelephoneNumberComment
            also mainEmailAddress in validateOptional (String255.Of (nameof organization.MainEmailAddress |> onBasePath)) organization.MainEmailAddress
            also mainEmailAddressComment in validateOptional (String255.Of (nameof organization.MainEmailAddressComment |> onBasePath))  organization.MainEmailAddressComment
            also otherContactMethods in organization.OtherContactMethods |> List.mapi (fun index c -> ValidatedContactMethod.BasicValidate (sprintf "%s.[%i]" (nameof organization.OtherContactMethods) index |> onBasePath) c) |> Trial.sequence
            also bankAccounts in organization.BankAccounts |> List.mapi (fun index b -> ValidatedBankAccount.BasicValidate (sprintf "%s.[%i]" (nameof organization.BankAccounts) index |> onBasePath) b) |> Trial.sequence
            yield {
                OrganizationId = organization.OrganizationId
                BuildingId = organization.BuildingId
                UsesVatNumber = organization.UsesVatNumber
                OrganizationNumber = organizationNumber
                VatNumber = vatNumber
                VatNumberVerifiedOn = vatNumberVerifiedOn
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
        UseTwoFac: bool
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
                UseTwoFac = user.UseTwoFac
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
        FinancialYearId: Guid
        Description: string option
        Cost: Decimal
        VatRate: PositiveInt
        FinancialCategoryId: Guid
        BookingDate: DateTime //Date when booked
        DistributionKeyId: Guid
        OrganizationId: Guid
        OrganizationBankAccount: ValidatedBankAccount
        OrganizationInvoiceNumber: String64 option //Number @ supplier
        InvoiceDate: DateTimeOffset //Date on the invoice
        DueDate: DateTimeOffset //Due date of the invoice
    }
    static member Validate (invoice: Invoice) =
        trial {
            from vatRate in validatePositiveInt (nameof invoice.VatRate) invoice.VatRate
            also organizationBankAccount in ValidatedBankAccount.BasicValidate (nameof invoice.OrganizationBankAccount) invoice.OrganizationBankAccount
            also organizationInvoiceNumber in validateOptional (String64.Of (nameof invoice.OrganizationInvoiceNumber)) invoice.OrganizationInvoiceNumber
            yield {
                InvoiceId = invoice.InvoiceId
                BuildingId = invoice.BuildingId
                FinancialYearId = invoice.FinancialYear.FinancialYearId
                Description = invoice.Description
                Cost = invoice.Cost
                VatRate = vatRate
                FinancialCategoryId = invoice.FinancialCategory.FinancialCategoryId
                BookingDate = invoice.BookingDate
                DistributionKeyId = invoice.DistributionKey.DistributionKeyId
                OrganizationId = invoice.Organization.OrganizationId
                OrganizationInvoiceNumber = organizationInvoiceNumber
                OrganizationBankAccount = organizationBankAccount
                InvoiceDate = invoice.InvoiceDate
                DueDate = invoice.DueDate
            }
        }        
        |> Trial.toResult

type ValidatedFinancialYear =
    {
        FinancialYearId: Guid
        BuildingId: Guid
        Code: String32
        StartDate: DateTime
        EndDate: DateTime
        IsClosed: bool
    }
    static member Validate (year: FinancialYear): Result<ValidatedFinancialYear, (string * string) list> =
        let validateDatePeriod (startDate: DateTime, endDate: DateTime) =
            if endDate.Date < startDate.Date then
                Trial.ofError (nameof year.EndDate, "De einddatum mag niet vóór de begindatum liggen...")
            else
                Trial.Pass endDate

        trial {
            from code in String32.Of (nameof year.Code) year.Code
            also endDate in validateDatePeriod (year.StartDate, year.EndDate)
            yield {
                FinancialYearId = year.FinancialYearId
                BuildingId = year.BuildingId
                Code = code
                StartDate = year.StartDate
                EndDate = year.EndDate
                IsClosed = year.IsClosed
            }
        }
        |> Trial.toResult

type ValidatedFinancialCategory =
    {
        FinancialCategoryId: Guid
        BuildingId: Guid option
        Code: String32
        Description: String255
    }
    static member Validate (category: FinancialCategory): Result<ValidatedFinancialCategory, (string * string) list> =
        trial {
            from code in String32.Of (nameof category.Code) category.Code
            also description in String255.Of (nameof category.Description) category.Description
            yield {
                FinancialCategoryId = category.FinancialCategoryId
                BuildingId = category.BuildingId
                Code = code
                Description = description
            }
        }
        |> Trial.toResult