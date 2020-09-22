module Server.Blueprint.Behavior

open Giraffe
open System
open Shared.Read
open System.Security.Claims
open Server.Library
open Shared.Write
open Shared.Remoting
open Shared.ConstrainedTypes

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
        abstract GenerateNewRecoveryCodes: userId: Guid * email: string -> Async<string list>
        abstract AddFailedTwoFacAttempt: FailedTwoFacAttempt -> Async<unit>

        abstract GenerateChangePasswordToken: claims: Claim list * expiresAfter: TimeSpan -> string
        abstract ValidateChangePasswordToken: token: string -> ClaimsPrincipal option
        abstract GenerateUsernamePasswordToken: claims: Claim list * expiresAfter: TimeSpan -> string
        abstract ValidateUsernamePasswordToken: token: string -> ClaimsPrincipal option

        abstract AuthenticateUser: Message<string * string> -> Async<Result<User, AuthenticationError>>
        abstract UserWithEmailAddressExists: string -> Async<bool>
        abstract FindUserByEmailAddress: Message<string> -> Async<User option>
        abstract GetUser: Message<Guid> -> Async<User option>
        abstract GetNbFailedTwoFacAttempts: userId: Guid * after: DateTimeOffset -> Async<int>
        abstract HttpHandler: HttpHandler

module Media =
    open Shared.MediaLibrary

    [<NoComparison; NoEquality>]
    type IMediaSystem =
        //Commands
        abstract CreateMediaFile: Message<MediaFile> -> Async<unit>
        abstract DeleteMediaFilesForEntity: Message<Guid> -> Async<unit>
        abstract DeleteMediaFile: Message<Guid> -> Async<unit>

        //Queries
        abstract GetMediaFile: partition: string -> fileId: Guid -> Async<MediaFile option>
        abstract GetMediaFilesForEntities: partition: string -> entityIds: Guid list -> Async<MediaFile list>
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
        abstract GetBuildings: Message<unit> -> Async<BuildingListItem list>

module Lots =
    [<NoComparison; NoEquality>]
    type ILotSystem =
        //Commands
        abstract CreateLot: msg: Message<Lot> -> Async<Result<unit, SaveLotError>>       
        abstract UpdateLot: msg: Message<Lot> -> Async<Result<unit, SaveLotError>>
        abstract DeleteLot: msg: Message<BuildingId * Guid> -> Async<Result<unit, DeleteLotError>>

        //Queries
        abstract GetLot: lotId: Message<Guid> -> Async<Lot option>
        abstract GetLots: filter: Message<BuildingId> -> Async<LotListItem list>

module ProfessionalSyndics =
    [<NoComparison; NoEquality>]
    type IProfessionalSyndicSystem =
        //Commands
        abstract CreateProfessionalSyndic: msg: Message<ProfessionalSyndic> -> Async<Result<unit, SaveProfessionalSyndicError>>       
        abstract UpdateProfessionalSyndic: msg: Message<ProfessionalSyndic> -> Async<Result<unit, SaveProfessionalSyndicError>>
        abstract DeleteProfessionalSyndic: msg: Message<Guid> -> Async<Result<unit, DeleteProfessionalSyndicError>>

        //Queries
        abstract GetProfessionalSyndic: orgId: Message<Guid> -> Async<ProfessionalSyndic option>
        abstract GetProfessionalSyndics: Message<unit> -> Async<ProfessionalSyndicListItem list>

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
        abstract SaveContractTypeAnswer: Message<ContractTypeAnswer> -> Async<Result<unit, SaveAnswerError>>
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
        //Queries
        abstract GetDistributionKeys: Message<BuildingId> -> Async<DistributionKey list>
        abstract GetDistributionKeyListItems: Message<BuildingId> -> Async<DistributionKeyListItem list>
        abstract GetInvoices: Message<InvoiceFilter> -> Async<InvoiceListItem list>
        abstract GetInvoice: Message<Guid> -> Async<Invoice option>
        abstract GetFinancialYears: Message<BuildingId> -> Async<FinancialYear list>
        abstract GetFinancialCategories: Message<BuildingId> -> Async<FinancialCategory list>

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