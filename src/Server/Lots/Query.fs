module Server.Lots.Query

open System
open Shared.Read
open Server.Library

let dummyLots: Lot list = [
    {
        LotId = Guid.NewGuid()
        Code = "A101"
        BuildingId = Guid.NewGuid()
        CurrentOwner = None
        LotType = LotType.Appartment
        Description = Some "Appartement 101"
        Floor = Some 1
        Surface = Some 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "A102"
        BuildingId = Guid.NewGuid()
        CurrentOwner = None
        LotType = LotType.Appartment
        Description = Some "Appartement 102"
        Floor = Some 1
        Surface = Some 120
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "A201"
        BuildingId = Guid.NewGuid()
        CurrentOwner = None
        LotType = LotType.Appartment
        Description = Some "Appartement 201"
        Floor = Some 2
        Surface = Some 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "A202"
        BuildingId = Guid.NewGuid()
        CurrentOwner = None
        LotType = LotType.Appartment
        Description = Some "Appartement 202"
        Floor = Some 2
        Surface = Some 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "A301"
        BuildingId = Guid.NewGuid()
        CurrentOwner = None
        LotType = LotType.Appartment
        Description = Some "Appartement 301"
        Floor = Some 3
        Surface = Some 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "A302"
        BuildingId = Guid.NewGuid()
        CurrentOwner = None
        LotType = LotType.Appartment
        Description = Some "Appartement 302"
        Floor = Some 3
        Surface = Some 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "GB001"
        BuildingId = Guid.NewGuid()
        CurrentOwner = None
        LotType = LotType.Garage
        Description = Some "Garagebox 1"
        Floor = Some -1
        Surface = Some 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "GB002"
        BuildingId = Guid.NewGuid()
        CurrentOwner = None
        LotType = LotType.Garage
        Description = Some "Garagebox 2"
        Floor = Some -1
        Surface = Some 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "P001"
        BuildingId = Guid.NewGuid()
        CurrentOwner = None
        LotType = LotType.ParkingSpace
        Description = Some "Bovengrondse parkeerplaats 1 naast het gebouw"
        Floor = Some 0
        Surface = Some 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "P002"
        BuildingId = Guid.NewGuid()
        CurrentOwner = None
        LotType = LotType.ParkingSpace
        Description = Some "Bovengrondse parkeerplaats 2 naast het gebouw"
        Floor = Some 0
        Surface = Some 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "OHK001"
        BuildingId = Guid.NewGuid()
        CurrentOwner = None
        LotType = LotType.Storage
        Description = Some "Onderhoudskast op de gang"
        Floor = Some 1
        Surface = Some 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "W001"
        BuildingId = Guid.NewGuid()
        CurrentOwner = None
        LotType = LotType.CommercialProperty
        Description = Some "Winkeltje op het gelijkvloers - 'De Grote Honger'"
        Floor = Some 0
        Surface = Some 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "TUIN"
        BuildingId = Guid.NewGuid()
        CurrentOwner = None
        LotType = LotType.Other
        Description = Some "Gemeenschappelijke stadstuin"
        Floor = Some 0
        Surface = Some 100
        IsActive = true
    }
]

let mapCurrentOwner (lotOwner: LotOwner): LotOwnerListItem =
    match lotOwner with
    | LotOwner.Owner o -> LotOwnerListItem.Owner {| Name = o.Person.FullName; PersonId = o.Person.PersonId |}
    | LotOwner.Organization o -> LotOwnerListItem.Organization {| Name = o.Name; OrganizationId = o.OrganizationId |}

let toListItem (lot: Lot): LotListItem = {
    LotId = lot.LotId
    Code = lot.Code
    BuildingId = lot.BuildingId
    CurrentOwner = lot.CurrentOwner |> Option.map mapCurrentOwner
    LotType = lot.LotType
    Floor = lot.Floor
    Description = lot.Description
    IsActive = lot.IsActive
}

let getLots (connectionString: string) (filter: {| BuildingId: Guid |}) = async {
    return dummyLots |> List.map toListItem
}

let getLot (connectionString: string) (lotId: Guid) = async {
    return dummyLots |> List.tryFind (fun lot -> lot.LotId = lotId)
}