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
    static member FromBackendModel (lots: LotListItem list) (key: DistributionKey) = {
        DistributionKeyId = key.DistributionKeyId
        BuildingId = key.BuildingId
        Name = key.Name
        DistributionType = key.DistributionType
        CanBeEdited = key.BuildingId.IsSome
        MatchingLots =
            match key.LotsOrLotTypes with
            | LotsOrLotTypes.Lots lotIds -> lots |> List.filter (fun lot -> lotIds |> List.contains(lot.LotId))
            | LotsOrLotTypes.LotTypes types -> lots |> List.filter (fun lot -> (if not key.IncludeGroundFloor then lot.Floor.IsNone || lot.Floor.Value <> 0 else true) && types |> List.contains(lot.LotType))
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