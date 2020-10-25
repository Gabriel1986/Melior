module Server.SeedData.DistributionKeys

open System
open Shared.Read
open Server.Blueprint.Data.SeedData

let predefinedDistributionKeys: DistributionKeySeedRow seq = seq {
    {
        DistributionKeyId = Guid.NewGuid()
        BuildingId = None
        Name = "Totaal - volgens aandelen"
        DistributionType = DistributionType.Shares
        LotsOrLotTypes = LotsOrLotTypes.LotTypes (LotType.AllValues ())
        IncludeGroundFloor = true
    }
    {
        DistributionKeyId = Guid.NewGuid()
        BuildingId = None
        Name = "Appartementen en studio's - volgens aandelen"
        DistributionType = DistributionType.Shares
        LotsOrLotTypes = LotsOrLotTypes.LotTypes [ LotType.Appartment; LotType.Studio ]
        IncludeGroundFloor = true
    }
    {
        DistributionKeyId = Guid.NewGuid()
        BuildingId = None
        Name = "Appartementen en studio's exl. GLV - volgens aandelen"
        DistributionType = DistributionType.Shares
        LotsOrLotTypes = LotsOrLotTypes.LotTypes [ LotType.Appartment; LotType.Studio ]
        IncludeGroundFloor = false
    }
    {
        DistributionKeyId = Guid.NewGuid()
        BuildingId = None
        Name = "Garages en staanplaatsen - volgens aandelen"
        DistributionType = DistributionType.Shares
        LotsOrLotTypes = LotsOrLotTypes.LotTypes [ LotType.Garage; LotType.ParkingSpace ]
        IncludeGroundFloor = true
    }
    {
        DistributionKeyId = Guid.NewGuid()
        BuildingId = None
        Name = "Bergingen en kelders - volgens aandelen"
        DistributionType = DistributionType.Shares
        LotsOrLotTypes = LotsOrLotTypes.LotTypes [ LotType.Storage; LotType.Garage ]
        IncludeGroundFloor = true
    }
    {
        DistributionKeyId = Guid.NewGuid()
        BuildingId = None
        Name = "Totaal - volgens gelijke delen"
        DistributionType = DistributionType.EqualParts
        LotsOrLotTypes = LotsOrLotTypes.LotTypes (LotType.AllValues ())
        IncludeGroundFloor = true
    }
    {
        DistributionKeyId = Guid.NewGuid()
        BuildingId = None
        Name = "Appartementen en studio's - volgens gelijke delen"
        DistributionType = DistributionType.EqualParts
        LotsOrLotTypes = LotsOrLotTypes.LotTypes [ LotType.Appartment; LotType.Studio ]
        IncludeGroundFloor = true
    }
    {
        DistributionKeyId = Guid.NewGuid()
        BuildingId = None
        Name = "Appartementen en studio's exl. GLV - volgens gelijke delen"
        DistributionType = DistributionType.EqualParts
        LotsOrLotTypes = LotsOrLotTypes.LotTypes [ LotType.Appartment; LotType.Studio ]
        IncludeGroundFloor = false
    }
    {
        DistributionKeyId = Guid.NewGuid()
        BuildingId = None
        Name = "Garages en staanplaatsen - volgens gelijke delen"
        DistributionType = DistributionType.EqualParts
        LotsOrLotTypes = LotsOrLotTypes.LotTypes [ LotType.Garage; LotType.ParkingSpace ]
        IncludeGroundFloor = true
    }
    {
        DistributionKeyId = Guid.NewGuid()
        BuildingId = None
        Name = "Bergingen en kelders - volgens gelijke delen"
        DistributionType = DistributionType.EqualParts
        LotsOrLotTypes = LotsOrLotTypes.LotTypes [ LotType.Storage; LotType.Garage ]
        IncludeGroundFloor = true
    }
}