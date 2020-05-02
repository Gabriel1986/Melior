namespace Server.Lots

open System
open Shared.Domain
open Server.Library

module Query =
    let dummyLots: Lot list = [
        {
            LotId = Guid.NewGuid()
            Code = "A101"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
            LotType = LotType.Appartment
            Description = Some "Appartement 101"
            Floor = 1
            Surface = 100
            IsActive = true
        }
        {
            LotId = Guid.NewGuid()
            Code = "A102"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
            LotType = LotType.Appartment
            Description = Some "Appartement 102"
            Floor = 1
            Surface = 120
            IsActive = true
        }
        {
            LotId = Guid.NewGuid()
            Code = "A201"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
            LotType = LotType.Appartment
            Description = Some "Appartement 201"
            Floor = 2
            Surface = 100
            IsActive = true
        }
        {
            LotId = Guid.NewGuid()
            Code = "A202"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
            LotType = LotType.Appartment
            Description = Some "Appartement 202"
            Floor = 2
            Surface = 100
            IsActive = true
        }
        {
            LotId = Guid.NewGuid()
            Code = "A301"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
            LotType = LotType.Appartment
            Description = Some "Appartement 301"
            Floor = 3
            Surface = 100
            IsActive = true
        }
        {
            LotId = Guid.NewGuid()
            Code = "A302"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
            LotType = LotType.Appartment
            Description = Some "Appartement 302"
            Floor = 3
            Surface = 100
            IsActive = true
        }
        {
            LotId = Guid.NewGuid()
            Code = "B001"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
            LotType = LotType.Basement
            Description = Some "Gemeenschappelijke kelder"
            Floor = -1
            Surface = 100
            IsActive = true
        }
        {
            LotId = Guid.NewGuid()
            Code = "G001"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
            LotType = LotType.Garage
            Description = Some "Gemeenschappelijke garage, zonder boxen"
            Floor = -1
            Surface = 100
            IsActive = true
        }
        {
            LotId = Guid.NewGuid()
            Code = "GB001"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
            LotType = LotType.Garage
            Description = Some "Garagebox 1"
            Floor = -1
            Surface = 100
            IsActive = true
        }
        {
            LotId = Guid.NewGuid()
            Code = "GB002"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
            LotType = LotType.Garage
            Description = Some "Garagebox 2"
            Floor = -1
            Surface = 100
            IsActive = true
        }
        {
            LotId = Guid.NewGuid()
            Code = "P001"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
            LotType = LotType.ParkingSpace
            Description = Some "Bovengrondse parkeerplaatsen naast het gebouw"
            Floor = 0
            Surface = 100
            IsActive = true
        }
        {
            LotId = Guid.NewGuid()
            Code = "OHK001"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
            LotType = LotType.Storage
            Description = Some "Onderhoudskast op de gang"
            Floor = 1
            Surface = 100
            IsActive = true
        }
        {
            LotId = Guid.NewGuid()
            Code = "W001"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
            LotType = LotType.Store
            Description = Some "Winkeltje op het gelijkvloers - 'De Grote Honger'"
            Floor = 0
            Surface = 100
            IsActive = true
        }
        {
            LotId = Guid.NewGuid()
            Code = "V001"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
            LotType = LotType.PlaceOfBusiness
            Description = Some "Vergaderruimte 'Flargenta'"
            Floor = 4
            Surface = 100
            IsActive = false
        }
        {
            LotId = Guid.NewGuid()
            Code = "TUIN"
            Building = {| BuildingId = Guid.NewGuid(); Code = "B01" |}
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
        LotType = lot.LotType
        Floor = lot.Floor
        Description = lot.Description
        IsActive = lot.IsActive
    }


    let getLotsForBuilding connectionString (msg: Message<Guid>) = async {
        return dummyLots |> List.map toListItem
    }

    let getLots (connectionString: string) (message: Message<unit>) = async {
        return dummyLots |> List.map toListItem
    }

    let getLot (connectionString: string) (message: Message<Guid>) = async {
        return dummyLots |> List.tryFind (fun lot -> lot.LotId = message.Payload)
    }
