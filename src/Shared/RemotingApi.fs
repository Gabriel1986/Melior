module Shared.Remoting

open System
open Shared.Domain
open Shared.Buildings

[<NoComparison; NoEquality>]
type RemotingApi = {
    GetCurrentUser: unit                  -> Async<CurrentUser>

    CreateBuilding:     CreateBuildingRequest -> Async<Result<unit, InvariantError>>
    UpdateBuilding:     UpdateBuildingRequest -> Async<Result<unit, InvariantError>>
    DeleteBuilding:     Guid                  -> Async<Result<unit, InvariantError>>
    GetBuilding:        Guid                  -> Async<Building option>
    GetBuildings:       unit                  -> Async<BuildingListItem list>
    GetLotsForBuilding: Guid                  -> Async<LotListItem list>
    GetResidentsForBuilding: Guid             -> Async<ResidentListItem list>

    DeleteLot: Guid -> Async<Result<unit, InvariantError>>
    GetLot:    Guid -> Async<Lot option>
    GetLots:   unit -> Async<LotListItem list>
        
    DeleteResident: Guid -> Async<Result<unit, InvariantError>>
    GetResident   : Guid -> Async<Resident option>
    GetResidents  : unit -> Async<ResidentListItem list>
}

let routeBuilder = sprintf "/remoting/%s/%s"