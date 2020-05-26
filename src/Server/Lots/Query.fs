module Server.Lots.Query

open System
open Shared.Domain
open Server.Library

let dummyLots: Lot list = [
    {
        LotId = Guid.NewGuid()
        Code = "A101"
        Building = {| BuildingId = Guid.NewGuid(); Name = "Building 1" |}
        CurrentOwner = Person {| FirstName = "Jefke"; LastName = "Janssens"; PersonId = Guid.NewGuid() |}
        LotType = LotType.Appartment
        Description = Some "Appartement 101"
        Floor = 1
        Surface = 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "A102"
        Building = {| BuildingId = Guid.NewGuid(); Name = "Building 1" |}
        CurrentOwner = Person {| FirstName = "Jefke"; LastName = "Janssens"; PersonId = Guid.NewGuid() |}
        LotType = LotType.Appartment
        Description = Some "Appartement 102"
        Floor = 1
        Surface = 120
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "A201"
        Building = {| BuildingId = Guid.NewGuid(); Name = "Building 1" |}
        CurrentOwner = Person {| FirstName = "Jefke"; LastName = "Janssens"; PersonId = Guid.NewGuid() |}
        LotType = LotType.Appartment
        Description = Some "Appartement 201"
        Floor = 2
        Surface = 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "A202"
        Building = {| BuildingId = Guid.NewGuid(); Name = "Building 1" |}
        CurrentOwner = Person {| FirstName = "Jefke"; LastName = "Janssens"; PersonId = Guid.NewGuid() |}
        LotType = LotType.Appartment
        Description = Some "Appartement 202"
        Floor = 2
        Surface = 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "A301"
        Building = {| BuildingId = Guid.NewGuid(); Name = "Building 1" |}
        CurrentOwner = Person {| FirstName = "Jefke"; LastName = "Janssens"; PersonId = Guid.NewGuid() |}
        LotType = LotType.Appartment
        Description = Some "Appartement 301"
        Floor = 3
        Surface = 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "A302"
        Building = {| BuildingId = Guid.NewGuid(); Name = "Building 1" |}
        CurrentOwner = Person {| FirstName = "Jefke"; LastName = "Janssens"; PersonId = Guid.NewGuid() |}
        LotType = LotType.Appartment
        Description = Some "Appartement 302"
        Floor = 3
        Surface = 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "GB001"
        Building = {| BuildingId = Guid.NewGuid(); Name = "Building 1" |}
        CurrentOwner = Person {| FirstName = "Jefke"; LastName = "Janssens"; PersonId = Guid.NewGuid() |}
        LotType = LotType.Garage
        Description = Some "Garagebox 1"
        Floor = -1
        Surface = 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "GB002"
        Building = {| BuildingId = Guid.NewGuid(); Name = "Building 1" |}
        CurrentOwner = Person {| FirstName = "Jefke"; LastName = "Janssens"; PersonId = Guid.NewGuid() |}
        LotType = LotType.Garage
        Description = Some "Garagebox 2"
        Floor = -1
        Surface = 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "P001"
        Building = {| BuildingId = Guid.NewGuid(); Name = "Building 1" |}
        CurrentOwner = Person {| FirstName = "Jefke"; LastName = "Janssens"; PersonId = Guid.NewGuid() |}
        LotType = LotType.ParkingSpace
        Description = Some "Bovengrondse parkeerplaats 1 naast het gebouw"
        Floor = 0
        Surface = 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "P002"
        Building = {| BuildingId = Guid.NewGuid(); Name = "Building 1" |}
        CurrentOwner = Person {| FirstName = "Jefke"; LastName = "Janssens"; PersonId = Guid.NewGuid() |}
        LotType = LotType.ParkingSpace
        Description = Some "Bovengrondse parkeerplaats 2 naast het gebouw"
        Floor = 0
        Surface = 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "OHK001"
        Building = {| BuildingId = Guid.NewGuid(); Name = "Building 1" |}
        CurrentOwner = Person {| FirstName = "Jefke"; LastName = "Janssens"; PersonId = Guid.NewGuid() |}
        LotType = LotType.Storage
        Description = Some "Onderhoudskast op de gang"
        Floor = 1
        Surface = 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "W001"
        Building = {| BuildingId = Guid.NewGuid(); Name = "Building 1" |}
        CurrentOwner = Person {| FirstName = "Jefke"; LastName = "Janssens"; PersonId = Guid.NewGuid() |}
        LotType = LotType.CommercialProperty
        Description = Some "Winkeltje op het gelijkvloers - 'De Grote Honger'"
        Floor = 0
        Surface = 100
        IsActive = true
    }
    {
        LotId = Guid.NewGuid()
        Code = "TUIN"
        Building = {| BuildingId = Guid.NewGuid(); Name = "Building 1" |}
        CurrentOwner = Person {| FirstName = "Jefke"; LastName = "Janssens"; PersonId = Guid.NewGuid() |}
        LotType = LotType.Other
        Description = Some "Gemeenschappelijke stadstuin"
        Floor = 0
        Surface = 100
        IsActive = true
    }
]

let toListItem (lot: Lot): LotListItem = {
    LotId = lot.LotId
    Code = lot.Code
    Building = lot.Building
    CurrentOwner = lot.CurrentOwner
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
