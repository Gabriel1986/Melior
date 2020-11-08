module Client.Financial.DistributionKeys.Types

open System
open Shared.Read
open Shared.Write

type DistributionKeyModel = 
    {
        DistributionKeyId: Guid
        BuildingId: Guid option
        Name: string
        DistributionType: DistributionType
        CanBeEdited: bool
        MatchingLots: LotListItem list
    }
    static member Init (buildingId: BuildingId option) = {
        DistributionKeyId = Guid.NewGuid()
        BuildingId = buildingId
        Name = ""
        DistributionType = DistributionType.Shares
        CanBeEdited = true
        MatchingLots = []
    }
    member me.ToBackendModel () = {
        DistributionKeyId = me.DistributionKeyId
        BuildingId = me.BuildingId
        Name = me.Name
        DistributionType = me.DistributionType
        LotsOrLotTypes = LotsOrLotTypes.Lots (me.MatchingLots |> List.map (fun lot -> lot.LotId))
        IncludeGroundFloor = true
    }
    member me.Validate () =
        ValidatedDistributionKey.Validate (me.ToBackendModel ())