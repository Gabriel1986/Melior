module Shared.Remoting

open System
open Shared.Domain
open Shared.Buildings

type RemotingApi = {
    GetCurrentUser: unit                  -> Async<CurrentUser>

    CreateBuilding: CreateBuildingRequest -> Async<Result<unit, InvariantError>>
    UpdateBuilding: UpdateBuildingRequest -> Async<Result<unit, InvariantError>>
    DeleteBuilding: Guid                  -> Async<Result<unit, InvariantError>>
    GetBuilding:    Guid                  -> Async<Building option>
    GetBuildings:   unit                  -> Async<BuildingListItem list>

    //CreateLot: Lot  -> Async<Result<unit, ApplicationError>>
    //UpdateLot: Lot  -> Async<Result<unit, ApplicationError>>
    //DeleteLot: Guid -> Async<Result<unit, AuthorizationError>>
    //GetLot:    Guid -> Async<Lot option>
    //GetLots:   unit -> Async<LotListItem list>
        
    //CreateResident: Resident       -> Async<Result<unit, ApplicationError>>
    //UpdateResident: Resident       -> Async<Result<unit, ApplicationError>>
    //DeleteResident: Guid           -> Async<Result<unit, AuthorizationError>>
    //GetResident   : Guid           -> Async<Resident option>
    //GetResidents  : ResidentFilter -> Async<ResidentListItem list>
}
//and ResidentFilter = {
//    BuildingId: Guid
//    IsActive  : bool
//}
let routeBuilder = sprintf "/remoting/%s/%s"