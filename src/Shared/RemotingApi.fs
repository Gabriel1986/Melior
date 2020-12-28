module Shared.Remoting

open System
open Shared.Read
open Shared.Write
open Shared.ConstrainedTypes

type AuthorizationError = AuthorizationError

type SaveBuildingError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteBuildingError =
    | AuthorizationError
    | NotFound
    
type SaveLotError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteLotError =
    | AuthorizationError
    | NotFound

type SaveOwnerError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteOwnerError =
    | AuthorizationError
    | NotFound

type SaveProfessionalSyndicError =    
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteProfessionalSyndicError =
    | AuthorizationError
    | NotFound

type SaveOrganizationTypeError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteOrganizationTypeError =
    | AuthorizationError
    | NotFound

type SaveContactPersonError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteContactPersonError =
    | AuthorizationError
    | NotFound

type SaveOrganizationError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteOrganizationError =
    | AuthorizationError
    | NotFound

type SaveAnswerError =    
    | AuthorizationError

type SaveContractError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteContractError =
    | AuthorizationError
    | NotFound

type SaveDistributionKeyError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteDistributionKeyError =
    | AuthorizationError
    | NotFound

type SaveInvoiceError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteInvoiceError =
    | AuthorizationError
    | NotFound

type SaveInvoicePaymentError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteInvoicePaymentError =
    | AuthorizationError
    | NotFound

type SaveUserError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteUserError =
    | AuthorizationError
    | NotFound

type SaveFinancialYearError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteFinancialYearError =
    | AuthorizationError
    | NotFound

type SaveFinancialTransactionError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteFinancialTransactionError =
    | AuthorizationError
    | NotFound

type SaveFinancialCategoryError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteFinancialCategoryError =
    | AuthorizationError
    | NotFound

type OwnerId = {
    PersonId: Guid
    BuildingId: Guid
}

[<NoComparison; NoEquality>]
type RemotingApi = {
    GetCurrentUser: unit -> Async<User>

    CreateBuilding: Building -> Async<Result<unit, SaveBuildingError>>
    UpdateBuilding: Building -> Async<Result<unit, SaveBuildingError>>
    DeleteBuilding: BuildingId -> Async<Result<unit, DeleteBuildingError>>
    GetBuilding: BuildingId -> Async<Building option>
    GetBuildings: BuildingId list option -> Async<BuildingListItem list>
    UpdateBuildingSyndic: BuildingId * SyndicInput option    -> Async<Result<unit, SaveBuildingError>>
    UpdateBuildingConcierge: BuildingId * ConciergeInput option -> Async<Result<unit, SaveBuildingError>>

    CreateLot: Lot -> Async<Result<unit, SaveLotError>>
    UpdateLot: Lot -> Async<Result<unit, SaveLotError>>
    DeleteLot: BuildingId * Guid -> Async<Result<unit, DeleteLotError>>
    GetLot: Guid -> Async<Lot option>
    GetLots: BuildingId -> Async<LotListItem list>
    
    CreateOwner: Owner -> Async<Result<unit, SaveOwnerError>>
    UpdateOwner: Owner -> Async<Result<unit, SaveOwnerError>>
    DeleteOwner: BuildingId * Guid -> Async<Result<unit, DeleteOwnerError>>
    GetOwner: Guid -> Async<Owner option>
    GetOwners: BuildingId -> Async<OwnerListItem list>

    CreateOrganization: Organization -> Async<Result<unit, SaveOrganizationError>>
    UpdateOrganization: Organization -> Async<Result<unit, SaveOrganizationError>>
    DeleteOrganization: BuildingId option * Guid -> Async<Result<unit, DeleteOrganizationError>>
    GetOrganization: Guid -> Async<Organization option>
    GetOrganizations: BuildingId -> Async<OrganizationListItem list>
    VerifyVatNumber: VatNumber -> Async<Result<VatNumberValidationResponse, string>>

    CreateProfessionalSyndic: ProfessionalSyndic -> Async<Result<unit, SaveProfessionalSyndicError>>
    UpdateProfessionalSyndic: ProfessionalSyndic -> Async<Result<unit, SaveProfessionalSyndicError>>
    DeleteProfessionalSyndic: Guid -> Async<Result<unit, DeleteProfessionalSyndicError>>
    GetProfessionalSyndics: Guid list option -> Async<ProfessionalSyndicListItem list>
    GetProfessionalSyndic: Guid -> Async<ProfessionalSyndic option>

    GetOrganizationTypes: unit -> Async<OrganizationType list>
    CreateOrganizationType: OrganizationType -> Async<Result<unit, SaveOrganizationTypeError>>
    UpdateOrganizationType: OrganizationType -> Async<Result<unit, SaveOrganizationTypeError>>
    DeleteOrganizationType: Guid -> Async<Result<unit, DeleteOrganizationTypeError>>

    GetContractTypeAnswers: BuildingId -> Async<ContractTypeAnswer list>
    SetContractTypeAnswers: ContractTypeAnswer list -> Async<Result<unit, SaveAnswerError>>
    GetContracts: BuildingId -> Async<Contract list>
    CreateContract: Contract -> Async<Result<unit, SaveContractError>>
    UpdateContract: Contract -> Async<Result<unit, SaveContractError>>
    DeleteContract: BuildingId * Guid -> Async<Result<unit, DeleteContractError>>

    GetDistributionKeys: BuildingId -> Async<DistributionKey list>
    GetDistributionKey: Guid -> Async<DistributionKey option>
    GetDistributionKeyListItems: BuildingId -> Async<DistributionKeyListItem list>
    CreateDistributionKey: DistributionKey -> Async<Result<unit, SaveDistributionKeyError>>
    UpdateDistributionKey: DistributionKey -> Async<Result<unit, SaveDistributionKeyError>>
    DeleteDistributionKey: BuildingId * Guid -> Async<Result<unit, DeleteDistributionKeyError>>

    GetInvoices: FinancialTransactionFilter -> Async<InvoiceListItem list>
    GetInvoice: Guid -> Async<Invoice option>
    CreateInvoice: Invoice -> Async<Result<unit, SaveInvoiceError>>
    UpdateInvoice: Invoice -> Async<Result<unit, SaveInvoiceError>>
    DeleteInvoice: BuildingId * Guid -> Async<Result<unit, DeleteInvoiceError>>

    CreateInvoicePayment: InvoicePaymentInput -> Async<Result<unit, SaveInvoicePaymentError>>
    UpdateInvoicePayment: InvoicePaymentInput -> Async<Result<unit, SaveInvoicePaymentError>>
    DeleteInvoicePayment: BuildingId * Guid -> Async<Result<unit, DeleteInvoicePaymentError>>

    GetFinancialYears: BuildingId -> Async<FinancialYear list>
    CloseFinancialYear: BuildingId * Guid -> Async<Result<unit, SaveFinancialYearError>>
    CreateFinancialYear: FinancialYear -> Async<Result<unit, SaveFinancialYearError>>
    UpdateFinancialYear: FinancialYear -> Async<Result<unit, SaveFinancialYearError>>

    GetFinancialTransactions: FinancialTransactionFilter -> Async<FinancialTransaction list>
    //CreateFinancialTransaction: FinancialTransaction -> Async<Result<unit, SaveFinancialTransactionError>>
    //UpdateFinancialTransaction: FinancialTransaction -> Async<Result<unit, SaveFinancialTransactionError>>
    //DeleteFinancialTransaction: BuildingId * Guid -> Async<Result<unit, DeleteFinancialTransactionError>>

    GetFinancialCategories: BuildingId -> Async<FinancialCategory list>
    CreateFinancialCategory: FinancialCategory -> Async<Result<unit, SaveFinancialCategoryError>>
    UpdateFinancialCategory: FinancialCategory -> Async<Result<unit, SaveFinancialCategoryError>>
    DeleteFinancialCategory: BuildingId * Guid -> Async<Result<unit, DeleteFinancialCategoryError>>

    GetUsers: unit -> Async<User list>
    GetUser: Guid -> Async<User option>
    CreateUser: User -> Async<Result<unit, SaveUserError>>
    UpdateUser: User -> Async<Result<unit, SaveUserError>>
    DeleteUser: Guid -> Async<Result<unit, DeleteUserError>>

    GetWarnings: BuildingId -> Async<Warning list>
}

let routeBuilder = sprintf "/remoting/%s/%s"