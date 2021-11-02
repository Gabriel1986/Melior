module Server.Blueprint.Behavior

open Giraffe
open System
open System.Security.Claims
open Shared.Read
open Shared.Write
open Shared.Remoting
open Shared.ConstrainedTypes
open Server.Library

module Authentication =
    open Server.Blueprint.Data.Authentication

    [<NoComparison; NoEquality>]
    type IAuthenticationSystem =
        (* Used internally! *)
        abstract AddUser: user: UserInput -> Async<Result<unit, CreateUserError>>

        //Commands
        abstract ValidateTwoFactorPIN: userId: Guid * validationCode: string -> Async<bool>
        abstract UpdateTwoFacAuthentication: UpdateTwoFacAuthentication -> Async<unit>
        abstract ValidateRecoveryCode: userId: Guid * code: string -> Async<bool>
        abstract RemoveUsedRecoveryCode: userId: Guid * code: string -> Async<unit>
        abstract GenerateNewRecoveryCodes: userId: Guid -> Async<string list>
        abstract AddFailedTwoFacAttempt: FailedTwoFacAttempt -> Async<unit>

        abstract UpdatePassword: Message<Guid * string> -> Async<Result<unit, exn>>

        abstract GenerateChangePasswordToken: claims: Claim list * expiresAfter: TimeSpan -> string
        abstract ValidateChangePasswordToken: token: string -> ClaimsPrincipal option
        abstract GenerateUsernamePasswordToken: claims: Claim list * expiresAfter: TimeSpan -> string
        abstract ValidateUsernamePasswordToken: token: string -> ClaimsPrincipal option

        //Queries
        abstract AuthenticateUser: Message<string * string> -> Async<Result<User, AuthenticationError>>
        abstract UserWithEmailAddressExists: string -> Async<bool>
        abstract FindUserByEmailAddress: Message<string> -> Async<User option>
        abstract GetUser: Message<Guid> -> Async<User option>
        abstract GetNbFailedTwoFacAttempts: userId: Guid * after: DateTimeOffset -> Async<int>
        abstract HttpHandler: HttpHandler

        abstract GetUsers: Message<unit> -> Async<User list>
        abstract CreateUser: Message<User> -> Async<Result<unit, SaveUserError>>
        abstract UpdateUser: Message<User> -> Async<Result<unit, SaveUserError>>
        abstract DeleteUser: Message<Guid> -> Async<Result<unit, DeleteUserError>>

module Media =
    open Shared.MediaLibrary

    [<NoComparison; NoEquality>]
    type IMediaSystem =
        //Commands
        abstract CreateMediaFile: Message<MediaFile> -> Async<unit>
        abstract DeleteMediaFilesForEntity: Message<Guid> -> Async<unit>
        abstract DeleteMediaFile: Message<Guid> -> Async<unit>
        abstract RemoveTemporaryMediaFile: Message<Guid> -> Async<unit>

        //Queries
        abstract GetMediaFile: partition: string -> fileId: Guid -> Async<MediaFile option>
        abstract GetMediaFilesForEntities: partition: string -> entityIds: Guid list -> Async<MediaFile list>
        abstract GetAllMediaFiles: unit -> Async<MediaFile list>
        abstract HttpHandler: HttpHandler

module Buildings =
    [<NoComparison; NoEquality>]
    type IBuildingSystem =
        //Commands
        abstract CreateBuilding: msg: Message<Building> -> Async<Result<unit, SaveBuildingError>>        
        abstract UpdateBuilding: msg: Message<Building> -> Async<Result<unit, SaveBuildingError>>
        abstract DeleteBuilding: msg: Message<Guid> -> Async<Result<unit, DeleteBuildingError>>
        abstract UpdateBuildingSyndic: msg: Message<BuildingId * SyndicInput option> -> Async<Result<unit, SaveBuildingError>>
        abstract UpdateBuildingConcierge: msg: Message<BuildingId * ConciergeInput option> -> Async<Result<unit, SaveBuildingError>>

        //Queries
        abstract GetBuilding: buildingId: Message<Guid> -> Async<Building option>
        abstract GetBuildings: Message<Guid list option> -> Async<BuildingListItem list>

module Lots =
    [<NoComparison; NoEquality>]
    type ILotSystem =
        //Commands
        abstract CreateLot: msg: Message<Lot> -> Async<Result<unit, SaveLotError>>       
        abstract UpdateLot: msg: Message<Lot> -> Async<Result<unit, SaveLotError>>
        abstract DeleteLot: msg: Message<BuildingId * Guid> -> Async<Result<unit, DeleteLotError>>

        //Queries
        abstract GetLot: lotId: Message<Guid> -> Async<Lot option>
        abstract GetLots: buildingId: Message<BuildingId> -> Async<LotListItem list>
        abstract GetFinancialLotOwners: filter: Message<LotOwnerFilter> -> Async<FinancialLotOwner list>

        //Seeding
        abstract GenerateOGMReferences: unit -> Async<unit>

module ProfessionalSyndics =
    [<NoComparison; NoEquality>]
    type IProfessionalSyndicSystem =
        //Commands
        abstract CreateProfessionalSyndic: msg: Message<ProfessionalSyndic> -> Async<Result<unit, SaveProfessionalSyndicError>>       
        abstract UpdateProfessionalSyndic: msg: Message<ProfessionalSyndic> -> Async<Result<unit, SaveProfessionalSyndicError>>
        abstract DeleteProfessionalSyndic: msg: Message<Guid> -> Async<Result<unit, DeleteProfessionalSyndicError>>

        //Queries
        abstract GetProfessionalSyndic: orgId: Message<Guid> -> Async<ProfessionalSyndic option>
        abstract GetProfessionalSyndics: Message<Guid list option> -> Async<ProfessionalSyndicListItem list>

    [<NoComparison; NoEquality>]
    type IProfessionalSyndicCache =
        abstract ClearCache: unit -> unit
        abstract GetBuildingIdsForProfessionalSyndic: professionalSyndicId: Guid -> Guid list

module Organizations =
    [<NoComparison; NoEquality>]
    type IOrganizationSystem =
        //Commands
        abstract CreateOrganization: msg: Message<Organization> -> Async<Result<unit, SaveOrganizationError>>       
        abstract UpdateOrganization: msg: Message<Organization> -> Async<Result<unit, SaveOrganizationError>>
        abstract DeleteOrganization: msg: Message<BuildingId option * Guid> -> Async<Result<unit, DeleteOrganizationError>>

        abstract CreateContactPerson: Message<ContactPerson> -> Async<Result<unit, SaveContactPersonError>>
        abstract UpdateContactPerson: Message<ContactPerson> -> Async<Result<unit, SaveContactPersonError>>
        abstract DeleteContactPerson: Message<BuildingId option * Guid> -> Async<Result<unit, DeleteContactPersonError>>

        abstract CreateOrganizationType: Message<OrganizationType> -> Async<Result<unit, SaveOrganizationTypeError>>
        abstract UpdateOrganizationType: Message<OrganizationType> -> Async<Result<unit, SaveOrganizationTypeError>>
        abstract DeleteOrganizationType: Message<Guid> -> Async<Result<unit, DeleteOrganizationTypeError>>

        //Queries
        abstract GetOrganization: orgId: Message<Guid> -> Async<Organization option>
        abstract GetOrganizations: filter: Message<BuildingId> -> Async<OrganizationListItem list>
        abstract VerifyVatNumber: VatNumber -> Async<Result<VatNumberValidationResponse, string>>
        abstract GetOrganizationTypes: unit -> Async<OrganizationType list>

module Owners =
    [<NoComparison; NoEquality>]
    type IOwnerSystem =
        //Commands
        abstract CreateOwner: msg: Message<Owner> -> Async<Result<unit, SaveOwnerError>>
        abstract UpdateOwner: msg: Message<Owner> -> Async<Result<unit, SaveOwnerError>>
        abstract DeleteOwner: msg: Message<BuildingId * Guid> -> Async<Result<unit, DeleteOwnerError>>

        //Queries
        abstract GetOwner: Message<Guid> -> Async<Owner option>
        abstract GetOwners: Message<BuildingId> -> Async<OwnerListItem list>

module Contracts =
    [<NoComparison; NoEquality>]
    type IContractSystem =
        //Commands
        abstract SaveContractTypeAnswers: Message<ContractTypeAnswer list> -> Async<Result<unit, SaveAnswerError>>
        abstract CreateContract: Message<Contract> -> Async<Result<unit, SaveContractError>>
        abstract UpdateContract: Message<Contract> -> Async<Result<unit, SaveContractError>>
        abstract DeleteContract: Message<BuildingId * Guid> -> Async<Result<unit, DeleteContractError>>

        //Queries
        abstract GetContractTypeAnswers: Message<BuildingId> -> Async<ContractTypeAnswer list>
        abstract GetContracts: Message<BuildingId> -> Async<Contract list>

module Financial =
    [<NoComparison; NoEquality>]
    type IFinancialSystem =
        //Commands
        abstract CreateDistributionKey: Message<DistributionKey> -> Async<Result<unit, SaveDistributionKeyError>>
        abstract UpdateDistributionKey: Message<DistributionKey> -> Async<Result<unit, SaveDistributionKeyError>>
        abstract DeleteDistributionKey: Message<BuildingId * Guid> -> Async<Result<unit, DeleteDistributionKeyError>>

        abstract CreateInvoice: Message<Invoice> -> Async<Result<unit, SaveInvoiceError>>
        abstract UpdateInvoice: Message<Invoice> -> Async<Result<unit, SaveInvoiceError>>
        abstract DeleteInvoice: Message<BuildingId * Guid> -> Async<Result<unit, DeleteInvoiceError>>

        abstract CreateInvoicePayment: Message<InvoicePayment> -> Async<Result<unit, SaveInvoicePaymentError>>
        abstract UpdateInvoicePayment: Message<InvoicePayment> -> Async<Result<unit, SaveInvoicePaymentError>>
        abstract DeleteInvoicePayment: Message<BuildingId * Guid> -> Async<Result<unit, DeleteInvoicePaymentError>>

        abstract CreateDepositRequest: Message<DepositRequest> -> Async<Result<unit, SaveDepositRequestError>>
        abstract UpdateDepositRequest: Message<DepositRequest> -> Async<Result<unit, SaveDepositRequestError>>
        abstract DeleteDepositRequest: Message<BuildingId * Guid> -> Async<Result<unit, DeleteDepositRequestError>> 

        abstract CreateDeposit: Message<Deposit> -> Async<Result<unit, SaveDepositError>>
        abstract UpdateDeposit: Message<Deposit> -> Async<Result<unit, SaveDepositError>>
        abstract DeleteDeposit: Message<BuildingId * Guid> -> Async<Result<unit, DeleteDepositError>>

        abstract CreateFinancialYear: Message<FinancialYear> -> Async<Result<unit, SaveFinancialYearError>>
        abstract UpdateFinancialYear: Message<FinancialYear> -> Async<Result<unit, SaveFinancialYearError>>
        abstract CloseFinancialYear: Message<BuildingId * Guid> -> Async<Result<unit, SaveFinancialYearError>>
        abstract DeleteFinancialYear: Message<BuildingId * Guid> -> Async<Result<unit, DeleteFinancialYearError>>

        abstract CreateFinancialCategory: Message<FinancialCategory> -> Async<Result<unit, SaveFinancialCategoryError>>
        abstract UpdateFinancialCategory: Message<FinancialCategory> -> Async<Result<unit, SaveFinancialCategoryError>>
        abstract DeleteFinancialCategory: Message<BuildingId * Guid> -> Async<Result<unit, DeleteFinancialCategoryError>>

        //abstract CreateFinancialTransaction: Message<FinancialTransaction> -> Async<Result<unit, SaveFinancialTransactionError>>
        //abstract UpdateFinancialTransaction: Message<FinancialTransaction> -> Async<Result<unit, SaveFinancialTransactionError>>
        //abstract DeleteFinancialTransaction: Message<BuildingId * Guid> -> Async<Result<unit, DeleteFinancialTransactionError>>

        //Queries
        abstract GetDistributionKeys: Message<BuildingId> -> Async<DistributionKey list>
        abstract GetDistributionKey: Message<Guid> -> Async<DistributionKey option>
        abstract GetDistributionKeyListItems: Message<BuildingId> -> Async<DistributionKeyListItem list>
        abstract GetInvoices: Message<FinancialTransactionFilter> -> Async<InvoiceListItem list>
        abstract GetInvoice: Message<Guid> -> Async<Invoice option>
        abstract GetFinancialYears: Message<BuildingId> -> Async<FinancialYear list>
        abstract GetFinancialCategories: Message<BuildingId> -> Async<FinancialCategory list>
        abstract GetFinancialTransactions: Message<FinancialTransactionFilter> -> Async<FinancialTransaction list>
        abstract GetDepositRequests: Message<FinancialTransactionFilter> -> Async<DepositRequestListItem list>
        abstract GetDepositRequest: Message<Guid> -> Async<DepositRequest option>

        //Seeding
        abstract SeedDistributionKeys: DistributionKey list -> Async<unit>

module Warnings =
    [<NoComparison; NoEquality>]
    type IWarningSystem =
        abstract GetWarnings: BuildingId -> Async<Warning list>

module Storage =
    open Server.Blueprint.Data.Storage

    [<NoComparison; NoEquality>]
    type IReactiveBehavior =
        abstract ReactTo: Message<StorageEvent> -> Async<StorageEvent list>

    type IStorageEngine =
        abstract PersistTransactional: Message<StorageEvent> list -> Async<int>

[<NoComparison; NoEquality>]
type IEnv =
    abstract AuthenticationSystem: Authentication.IAuthenticationSystem
    abstract MediaSystem: Media.IMediaSystem
    abstract BuildingSystem: Buildings.IBuildingSystem
    abstract ProfessionalSyndicSystem: ProfessionalSyndics.IProfessionalSyndicSystem
    abstract LotSystem: Lots.ILotSystem
    abstract OrganizationSystem: Organizations.IOrganizationSystem
    abstract OwnerSystem: Owners.IOwnerSystem
    abstract ContractSystem: Contracts.IContractSystem
    abstract FinancialSystem: Financial.IFinancialSystem
    abstract WarningSystem: Warnings.IWarningSystem