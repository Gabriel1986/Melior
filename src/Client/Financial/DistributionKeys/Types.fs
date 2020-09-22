module Client.Financial.DistributionKeys.Types

open System
open Shared.Read

type DistributionKeyModel = 
    {
        DistributionKeyId: Guid
        BuildingId: Guid option
        Name: string
        DistributionType: DistributionType
        CanBeEdited: bool
        MatchingLots: LotListItem list
    }
    static member init (buildingId: BuildingId option) = {
        DistributionKeyId = Guid.NewGuid()
        BuildingId = buildingId
        Name = ""
        DistributionType = DistributionType.Shares
        CanBeEdited = true
        MatchingLots = []
    }

module DistributionKeyModel =
    let toBackendType (model: DistributionKeyModel): DistributionKey = {
        DistributionKeyId = model.DistributionKeyId
        BuildingId = model.BuildingId
        Name = model.Name
        DistributionType = model.DistributionType
        LotsOrLotTypes = LotsOrLotTypes.Lots (model.MatchingLots |> List.map (fun lot -> lot.LotId))
    }