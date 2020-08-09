module Shared.Remoting

open System
open Shared.Read
open Shared.Write
open Shared.ConstrainedTypes

type AuthorizationError = AuthorizationError

type BuildingValidationError = {
    GeneralError: string list
    CodeError: string list
    NameError: string list
    AddressError: string list
    GeneralMeetingDateError: string list
}

type PersonValidationError = {
    GeneralErrors: string list
    FirstNameErrors: string list
    LastNameErrors: string list
    LanguageCodeErrors: string list
    TitleErrors: string list
    MainTelephoneNumberErrors: string list
    MainTelephoneNumberCommentErrors: string list
    MainEmailAddressErrors: string list
    MainEmailAddressCommentErrors: string list
    MainAddressErrors: string list
    ContactAddressErrors: string list
    OtherContactMethodsErrors: string list
    OtherAddressesErrors: string list
}

type CreateBuildingError =
    | AuthorizationError
    | Validation of (string * string) list
    
type UpdateBuildingError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteBuildingError =
    | AuthorizationError
    | NotFound

type CreateLotError =
    | AuthorizationError
    | Validation of (string * string) list
    
type UpdateLotError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteLotError =
    | AuthorizationError
    | NotFound

type CreateOwnerError =
    | AuthorizationError
    | Validation of (string * string) list
    
type UpdateOwnerError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteOwnerError =
    | AuthorizationError
    | NotFound

type CreateProfessionalSyndicError =
    | AuthorizationError
    | Validation of (string * string) list
    
type UpdateProfessionalSyndicError =    
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteProfessionalSyndicError =
    | AuthorizationError
    | NotFound

type CreateOrganizationTypeError =
    | AuthorizationError
    | Validation of (string * string) list
    
type UpdateOrganizationTypeError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteOrganizationTypeError =
    | AuthorizationError
    | NotFound

type CreateContactPersonError =
    | AuthorizationError
    | Validation of (string * string) list
    
type UpdateContactPersonError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteContactPersonError =
    | AuthorizationError
    | NotFound

type CreateOrganizationError =
    | AuthorizationError
    | Validation of (string * string) list
    
type UpdateOrganizationError =
    | AuthorizationError
    | Validation of (string * string) list
    | NotFound

type DeleteOrganizationError =
    | AuthorizationError
    | NotFound

type OwnerId = {
    PersonId: Guid
    BuildingId: Guid
}

[<NoComparison; NoEquality>]
type RemotingApi = {
    GetCurrentUser: unit -> Async<User>

    CreateBuilding: Building -> Async<Result<unit, CreateBuildingError>>
    UpdateBuilding: Building -> Async<Result<unit, UpdateBuildingError>>
    DeleteBuilding: Guid -> Async<Result<unit, DeleteBuildingError>>
    GetBuilding: Guid -> Async<Building option>
    GetBuildings: unit -> Async<BuildingListItem list>
    UpdateBuildingSyndic: Guid * SyndicInput option    -> Async<Result<unit, UpdateBuildingError>>
    UpdateBuildingConcierge: Guid * ConciergeInput option -> Async<Result<unit, UpdateBuildingError>>

    CreateLot: Lot  -> Async<Result<unit, CreateLotError>>
    UpdateLot: Lot  -> Async<Result<unit, UpdateLotError>>
    DeleteLot: BuildingId * Guid -> Async<Result<unit, DeleteLotError>>
    GetLot: Guid -> Async<Lot option>
    GetLots: {| BuildingId: Guid |} -> Async<LotListItem list>
    
    CreateOwner: Owner -> Async<Result<unit, CreateOwnerError>>
    UpdateOwner: Owner -> Async<Result<unit, UpdateOwnerError>>
    DeleteOwner: BuildingId * Guid -> Async<Result<unit, DeleteOwnerError>>
    GetOwner: Guid -> Async<Owner option>
    GetOwners: {| BuildingId: Guid |} -> Async<OwnerListItem list>

    CreateOrganization: Organization -> Async<Result<unit, CreateOrganizationError>>
    UpdateOrganization: Organization -> Async<Result<unit, UpdateOrganizationError>>
    DeleteOrganization: BuildingId option * Guid -> Async<Result<unit, DeleteOrganizationError>>
    GetOrganization: Guid -> Async<Organization option>
    GetOrganizations: {| BuildingId: Guid |} -> Async<OrganizationListItem list>
    VerifyVatNumber: VatNumber -> Async<Result<VatNumberValidationResponse, string>>

    CreateProfessionalSyndic: ProfessionalSyndic -> Async<Result<unit, CreateProfessionalSyndicError>>
    UpdateProfessionalSyndic: ProfessionalSyndic -> Async<Result<unit, UpdateProfessionalSyndicError>>
    DeleteProfessionalSyndic: Guid -> Async<Result<unit, DeleteProfessionalSyndicError>>
    GetProfessionalSyndics: unit -> Async<ProfessionalSyndicListItem list>
    GetProfessionalSyndic: Guid -> Async<ProfessionalSyndic option>

    GetOrganizationTypes: unit -> Async<OrganizationType list>
    CreateOrganizationType: OrganizationType -> Async<Result<unit, CreateOrganizationTypeError>>
    UpdateOrganizationType: OrganizationType -> Async<Result<unit, UpdateOrganizationTypeError>>
    DeleteOrganizationType: Guid -> Async<Result<unit, DeleteOrganizationTypeError>>
}

let routeBuilder = sprintf "/remoting/%s/%s"