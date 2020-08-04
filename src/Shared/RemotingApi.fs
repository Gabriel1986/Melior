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
    | AuthorizationError of AuthorizationError

type UpdateBuildingError =
    | AuthorizationError of AuthorizationError
    | NotFound

type CreatePersonError = 
    | AuthorizationError of AuthorizationError

type UpdatePersonError =
    | AuthorizationError of AuthorizationError
    | NotFound

type CreateLotError =
    | AuthorizationError of AuthorizationError

type UpdateLotError =
    | AuthorizationError of AuthorizationError
    | NotFound

type CreateOwnerError =
    | AuthorizationError of AuthorizationError

type UpdateOwnerError =
    | AuthorizationError of AuthorizationError
    | NotFound

type CreateProfessionalSyndicError =
    | AuthorizationError of AuthorizationError

type UpdateProfessionalSyndicError =    
    | AuthorizationError of AuthorizationError
    | NotFound

type CreateOrganizationTypeError =
    | AuthorizationError of AuthorizationError

type UpdateOrganizationTypeError =
    | AuthorizationError of AuthorizationError
    | NotFound

type CreateOrganizationError =
    | AuthorizationError of AuthorizationError

type UpdateOrganizationError =
    | AuthorizationError of AuthorizationError
    | NotFound

type OwnerId = {
    PersonId: Guid
    BuildingId: Guid
}

[<NoComparison; NoEquality>]
type RemotingApi = {
    GetCurrentUser: unit                  -> Async<User>

    CreateBuilding: ValidatedBuilding  -> Async<Result<unit, CreateBuildingError>>
    UpdateBuilding: ValidatedBuilding  -> Async<Result<unit, UpdateBuildingError>>
    DeleteBuilding: Guid               -> Async<Result<unit, AuthorizationError>>
    GetBuilding:    Guid               -> Async<Building option>
    GetBuildings:   unit               -> Async<BuildingListItem list>
    UpdateBuildingSyndic:    Guid * SyndicId option    -> Async<unit>
    UpdateBuildingConcierge: Guid * ConciergeId option -> Async<unit>

    CreateLot: ValidatedLot -> Async<Result<unit, CreateLotError>>
    UpdateLot: ValidatedLot -> Async<Result<unit, UpdateLotError>>
    DeleteLot: Guid -> Async<Result<unit, AuthorizationError>>
    GetLot:    Guid -> Async<Lot option>
    GetLots:   {| BuildingId: Guid |} -> Async<LotListItem list>
    
    CreateOwner: ValidatedOwner -> Async<Result<unit, CreateOwnerError>>
    UpdateOwner: ValidatedOwner -> Async<Result<unit, UpdateOwnerError>>
    DeleteOwner: Guid -> Async<Result<unit, AuthorizationError>>
    GetOwner:    Guid -> Async<Owner option>
    GetOwners:   {| BuildingId: Guid |} -> Async<OwnerListItem list>

    CreateOrganization: ValidatedOrganization -> Async<Result<unit, CreateOrganizationError>>
    UpdateOrganization: ValidatedOrganization -> Async<Result<unit, UpdateOrganizationError>>
    DeleteOrganization: Guid -> Async<Result<unit, AuthorizationError>>
    GetOrganization:    Guid -> Async<Organization option>
    GetOrganizations:   {| BuildingId: Guid |} -> Async<OrganizationListItem list>
    VerifyVatNumber:    VatNumber -> Async<Result<VatNumberValidationResponse, string>>

    CreatePerson: ValidatedPerson -> Async<Result<unit, CreatePersonError>>
    UpdatePerson: ValidatedPerson -> Async<Result<unit, UpdatePersonError>>

    CreateProfessionalSyndic: ValidatedProfessionalSyndic -> Async<Result<unit, CreateProfessionalSyndicError>>
    UpdateProfessionalSyndic: ValidatedProfessionalSyndic -> Async<Result<unit, UpdateProfessionalSyndicError>>
    DeleteProfessionalSyndic: Guid -> Async<Result<unit, AuthorizationError>>
    GetProfessionalSyndics:   unit -> Async<ProfessionalSyndicListItem list>
    GetProfessionalSyndic:    Guid -> Async<ProfessionalSyndic option>

    GetOrganizationTypes: unit -> Async<OrganizationType list>
    CreateOrganizationType: ValidatedOrganizationType -> Async<Result<unit, CreateOrganizationTypeError>>
    UpdateOrganizationType: ValidatedOrganizationType -> Async<Result<unit, UpdateOrganizationTypeError>>
    DeleteOrganizationType: Guid -> Async<Result<unit, AuthorizationError>>
}

let routeBuilder = sprintf "/remoting/%s/%s"