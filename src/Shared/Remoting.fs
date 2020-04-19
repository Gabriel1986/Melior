namespace Shared
open System
open Shared.Library

module Remoting =
    type ApplicationError =
        | InvariantError of InvariantError list
        | AuthorizationError of AuthorizationError
    and InvariantError = {
        Path: string option
        Message: string
    }
    and AuthorizationError = string    

    type RemotingApi = {
        CreateBuilding: Building -> Async<Result<unit, ApplicationError>>
        UpdateBuilding: Building -> Async<Result<unit, ApplicationError>>
        DeleteBuilding: Guid     -> Async<Result<unit, AuthorizationError>>
        GetBuilding:    Guid     -> Async<Building option>
        GetBuildings:   unit     -> Async<BuildingListItem list>

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
    and ResidentFilter = {
        BuildingId: Guid
        IsActive  : bool
    }