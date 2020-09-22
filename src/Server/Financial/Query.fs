module Server.Financial.Query

open System
open Server.Library
open Shared.Read
open Npgsql.FSharp
open Server.PostgreSQL
open Server.PostgreSQL.Sql

type DistributionKeyRow = {
    DistributionKeyId: Guid
    BuildingId: BuildingId option
    Name: string
    DistributionType: DistributionType
    LotId: Guid option
    LotType: LotType option
}

let mapToLotsOrLotTypes (rows: DistributionKeyRow list): LotsOrLotTypes =
    let first = rows |> List.head
    match first.LotId, first.LotType with
    | None, None -> LotsOrLotTypes.Lots []
    | Some _, _  -> LotsOrLotTypes.Lots (rows |> List.choose (fun r -> r.LotId))
    | _, Some _  -> LotsOrLotTypes.LotTypes (rows |> List.choose (fun r -> r.LotType))

let readDistributionKey (reader: CaseInsensitiveRowReader) = 
    {
        DistributionKeyId = reader.uuid "DistributionKeyId"
        BuildingId = reader.uuidOrNone "BuildingId"
        Name = reader.string "Name"
        DistributionType =
            match reader.string "DistributionType" with
            | x when x = string DistributionType.EqualParts -> 
                DistributionType.EqualParts
            | _ -> 
                DistributionType.Shares
        LotId = reader.uuidOrNone "LotId"
        LotType = (reader.stringOrNone "LotType" |> Option.map LotType.OfString)
    }

let getDistributionKeys (conn: string) (buildingId: BuildingId): Async<DistributionKey list> = async {
    let! distributionKeyRows =
        Sql.connect conn
        |> Sql.query
            """
                SELECT
                    dKey.DistributionKeyId,
                    dKey.BuildingId,
                    dKey.Name,
                    dKey.DistributionType
                    link.LotId, 
                    link.LotType
                FROM DistributionKeys dKey
                LEFT JOIN DistributionKeyLotsOrLotTypes links WHERE link.DistributionKeyId = @DistributionKeyId
                WHERE (dKey.BuildingId = @BuildingId OR dKey.BuildingId IS NULL) AND dKey.IsActive = TRUE
            """
        |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
        |> Sql.read readDistributionKey

    return
        distributionKeyRows
        |> List.groupBy (fun row -> row.DistributionKeyId)
        |> List.map (fun (_, distributionKeys) ->
            let first = distributionKeys |> List.head
            {
                DistributionKeyId = first.DistributionKeyId
                BuildingId = first.BuildingId
                Name = first.Name
                DistributionType = first.DistributionType
                LotsOrLotTypes = distributionKeys |> mapToLotsOrLotTypes
            }    
        )
}