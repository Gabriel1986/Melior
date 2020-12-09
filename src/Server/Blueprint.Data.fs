module Server.Blueprint.Data

open System
open Thoth.Json.Net
open Serilog
open Shared.Read
open Shared.MediaLibrary
open Shared.Write
open Shared.ConstrainedTypes
open Shared
open Shared.Trial
open Shared.Trial.Control
open Server

module Contracts =
    type DbContractType =
        | OtherContractType of string
        | InsuranceContractType of {| Name: string; BrokerId: Guid option |}
        | PredefinedContractType of PredefinedContractType

module Financial =
    [<RequireQualifiedAccess>]
    module BankAccount =
        let private bankAccountEncoder = Encode.Auto.generateEncoderCached<BankAccount>()
        let private bankAccountDecoder: Decoder<BankAccount> =
            Decode.object(fun get -> {
                Description = get.Required.Field "Description" Decode.string
                IBAN = get.Required.Field "IBAN" Decode.string
                BIC = get.Required.Field "BIC" Decode.string
                Validated = get.Optional.Field "Validated" Decode.bool
            })
        let private bankAccountListEncoder = Encode.Auto.generateEncoderCached<BankAccount list>()
    
        let toJson (bankAccount: BankAccount) =
            Encode.toString 0 (bankAccountEncoder bankAccount)
    
        let fromJson (str: string) =
            match Decode.fromString bankAccountDecoder str with
            | Ok result -> 
                result
            | Error e -> 
                Log.Logger.Error (e)
                failwithf "Decoding bank account has failed."
    
        let listToJson (bankAccounts: BankAccount list) =
            Encode.toString 0 (bankAccountListEncoder bankAccounts)
    
        let listFromJson (str: string) =
            match Decode.fromString (Decode.list bankAccountDecoder) str with
            | Ok result ->
                result
            | Error e ->
                Log.Logger.Error (e)
                []

    [<RequireQualifiedAccess>]
    module ValidatedBankAccount =
        let toBankAccount (validated: ValidatedBankAccount): BankAccount = { 
            Description = match validated.Description with | Some d -> string d | None -> ""
            IBAN = match validated.IBAN with | Some iban -> string iban | None -> ""
            BIC = match validated.BIC with | Some bic -> string bic | None -> ""
            Validated = validated.Validated
        }

        let toJson (validated: ValidatedBankAccount): string =
            validated |> toBankAccount |> BankAccount.toJson

        let listToJson (validated: ValidatedBankAccount list): string =
            validated |> List.map toBankAccount |> BankAccount.listToJson

module Authentication =
    type UserInput = {
        UserId: Guid
        DisplayName: string
        EmailAddress: string
        Roles: Role list
        PreferredLanguageCode: string
        Password: string
    }    

    type ValidatedUserInput = 
        {
            UserId: Guid
            DisplayName: String255
            EmailAddress: String255
            Roles: Role list
            PreferredLanguageCode: String16
            PasswordHash: byte []
        }
        static member Validate (passwordPepper: string) (user: UserInput) =
            trial {
                from displayName in String255.Of (nameof user.DisplayName) user.DisplayName
                also emailAddress in String255.Of (nameof user.EmailAddress) user.EmailAddress
                also preferredLanguageCode in String16.Of (nameof user.PreferredLanguageCode) user.PreferredLanguageCode
                yield {
                    UserId = user.UserId
                    DisplayName = displayName
                    EmailAddress = emailAddress
                    Roles = user.Roles
                    PreferredLanguageCode = preferredLanguageCode
                    PasswordHash = Encryption.hashPassword passwordPepper user.Password
                }
            }
            |> Trial.toResult

    type UpdateTwoFacAuthentication = {
        UserId: Guid
        UseTwoFac: bool
        TwoFacSecret: string
        RecoveryCodes: string list
    }

    type EncryptedUpdateTwoFacAuthentication = 
        {
            UserId: Guid
            UseTwoFac: bool
            TwoFacSecret: byte[]
            RecoveryCodeHashes: byte[] list
        }
        static member EncryptRecoveryCode (recoveryCodePepper: string) (recoveryCode: string) =
            Encryption.hashPassword recoveryCodePepper recoveryCode
        static member Encrypt (twoFacPassword: string) (recoveryCodePepper: string) (updateTwoFacAuthentication: UpdateTwoFacAuthentication) = {
            UserId = updateTwoFacAuthentication.UserId
            UseTwoFac = updateTwoFacAuthentication.UseTwoFac
            TwoFacSecret = Encryption.encryptString (updateTwoFacAuthentication.TwoFacSecret, twoFacPassword)
            RecoveryCodeHashes = 
                updateTwoFacAuthentication.RecoveryCodes
                |> List.map (EncryptedUpdateTwoFacAuthentication.EncryptRecoveryCode recoveryCodePepper)
        }

    type FailedTwoFacAttempt = {
        UserId: Guid
        Timestamp: DateTimeOffset
    }

    type AuthenticationError =
        | PasswordNotValid
        | UserNotFound

    type CreateUserError =
        | ValidationError of (string * string) list
        | Unauthorized

module Storage =
    open Authentication

    type CUDEvent<'a> =
        | Created of 'a
        | Updated of 'a
        | Deleted of Guid

    type BuildingSpecificCUDEvent<'a> =
        | Created of 'a
        | Updated of 'a
        | Deleted of BuildingId * Guid

    type BuildingEvent =
        | BuildingEvent of CUDEvent<ValidatedBuilding>
        | SyndicWasUpdated of BuildingId * ValidatedSyndic option
        | ConciergeWasUpdated of BuildingId * ValidatedConcierge option        

    type OwnerEvent =
        | OwnerEvent of BuildingSpecificCUDEvent<ValidatedOwner>

    type OrganizationEvent =
        | OrganizationWasCreated of ValidatedOrganization
        | OrganizationWasUpdated of ValidatedOrganization
        | OrganizationWasDeleted of BuildingId option * organizationId: Guid
        | ContactPersonWasCreated of ValidatedContactPerson
        | ContactPersonWasUpdated of ValidatedContactPerson
        | ContactPersonWasDeleted of BuildingId option * contactPersonId: Guid
        | OrganizationTypeEvent of CUDEvent<ValidatedOrganizationType>

    type PersonEvent =
        | PersonEvent of CUDEvent<ValidatedPerson>
        ///Upsert
        | PersonWasSaved of ValidatedPerson

    type LotEvent =
        | LotEvent of BuildingSpecificCUDEvent<ValidatedLot>
        | LotOwnerEvent of CUDEvent<ValidatedLotOwner>

    type ProfessionalSyndicEvent =
        | ProfessionalSyndicEvent of CUDEvent<ValidatedProfessionalSyndic>

    type MediaEvent =
        | MediaFileWasCreated of MediaFile
        | MediaFileWasDeleted of mediaFileId: Guid
        | MediaOfEntityWasDeleted of entityId: Guid
        | MediaFileWasPersisted of mediaFileId: Guid
        | TemporaryFileWasDeleted of mediaFileId: Guid

    type ContractEvent =
        | ContractEvent of BuildingSpecificCUDEvent<ValidatedContract>
        | ContractTypeAnswersWereUpdated of ContractTypeAnswer list

    type FinancialEvent =
        | DistributionKeyEvent of BuildingSpecificCUDEvent<ValidatedDistributionKey>
        | InvoiceEvent of BuildingSpecificCUDEvent<ValidatedInvoice>
        | FinancialCategoryEvent of BuildingSpecificCUDEvent<ValidatedFinancialCategory>
        | FinancialYearEvent of BuildingSpecificCUDEvent<ValidatedFinancialYear>
        | FinancialYearWasClosed of ValidatedFinancialYear

    type UserEvent =
        | UserEvent of CUDEvent<ValidatedUser>
        | UserWasAdded of ValidatedUserInput
        | PasswordWasChanged of userId: Guid * passwordHash: byte []
        | TwoFactorAuthenticationWasUpdated of EncryptedUpdateTwoFacAuthentication
        | FailedTwoFacAttemptWasAdded of FailedTwoFacAttempt
        | RecoveryCodesWereUpdated of userId: Guid * hashedRecoveryCodes: (byte []) list
        | RecoveryCodeWasUsed of userId: Guid * hashedRecoveryCode: byte []

    type StorageEvent =
        | BuildingEvent of BuildingEvent
        | OwnerEvent of OwnerEvent
        | OrganizationEvent of OrganizationEvent
        | LotEvent of LotEvent
        | ProfessionalSyndicEvent of ProfessionalSyndicEvent
        | MediaEvent of MediaEvent
        | ContractEvent of ContractEvent
        | FinancialEvent of FinancialEvent
        | UserEvent of UserEvent
