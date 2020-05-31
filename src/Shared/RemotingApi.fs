module Shared.Remoting

open System
open Shared.Read
open Shared.Write

type AuthorizationError = string

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
    | ValidationError of PersonValidationError

type UpdatePersonError =
    | NotFound
    | ValidationError of PersonValidationError

type OwnerId = {
    PersonId: Guid
    BuildingId: Guid
}

[<NoComparison; NoEquality>]
type RemotingApi = {
    GetCurrentUser: unit                  -> Async<CurrentUser>

    CreateBuilding:       ValidatedBuilding -> Async<Result<unit, CreateBuildingError>>
    UpdateBuilding:       ValidatedBuilding -> Async<Result<unit, UpdateBuildingError>>
    DeleteBuilding:       Guid            -> Async<Result<unit, AuthorizationError>>
    GetBuilding:          Guid            -> Async<Building option>
    GetBuildings:         unit            -> Async<BuildingListItem list>

    DeleteLot: Guid -> Async<Result<unit, AuthorizationError>>
    GetLot:    Guid -> Async<Lot option>
    GetLots:   {| BuildingId: Guid |} -> Async<LotListItem list>
    
    DeleteOwner: Guid -> Async<Result<unit, AuthorizationError>>
    GetOwner:    Guid -> Async<Owner option>
    GetOwners:   {| BuildingId: Guid |} -> Async<OwnerListItem list>

    DeleteOrganization: Guid -> Async<Result<unit, AuthorizationError>>
    GetOrganization:    Guid -> Async<Organization option>
    GetOrganizations:   {| BuildingId: Guid |} -> Async<OrganizationListItem list>

    CreatePerson: ValidatedPerson -> Async<Result<unit, CreatePersonError>>
    UpdatePerson: ValidatedPerson -> Async<Result<unit, UpdatePersonError>>

    GetProfessionalSyndics: unit -> Async<ProfessionalSyndicListItem list>
    GetProfessionalSyndic: Guid -> Async<ProfessionalSyndic option>
}

let routeBuilder = sprintf "/remoting/%s/%s"