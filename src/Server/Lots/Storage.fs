module Server.Lots.Storage

open System
open Npgsql.FSharp
open Server.PostgreSQL
open Shared.Read
open Shared.Write

[<NoComparison; NoEquality>]
type ILotStorage =
    abstract CreateLot: ValidatedLot -> Async<unit>
    abstract UpdateLot: ValidatedLot -> Async<int>
    abstract DeleteLot: BuildingId * lotId: Guid  -> Async<int>

let private paramsFor (validated: ValidatedLot) =
    [
        "@LotId"                     , Sql.uuid validated.LotId
        "@BuildingId"                , Sql.uuid validated.BuildingId
        "@Code"                      , Sql.string (string validated.Code)
        "@LotType"                   , Sql.string (string validated.LotType)
        "@Description"               , Sql.stringOrNone validated.Description
        "@Floor"                     , Sql.intOrNone validated.Floor
        "@Surface"                   , Sql.intOrNone (validated.Surface |> Option.map (fun s -> s.Value))
    ]

let createLot (connectionString: string) (validated: ValidatedLot) =
    //TODO: store owners
    Sql.connect connectionString
    |> Sql.query
        """
            INSERT INTO Lots (
                LotId,
                BuildingId,
                CurrentOwnerPersonId,
                CurrentOwnerOrganizationId,
                Code,
                LotType,
                Description,
                Floor,
                Surface
            ) VALUES (
                @LotId,
                @BuildingId,
                @CurrentOwnerPersonId,
                @CurrentOwnerOrganizationId,
                @Code,
                @LotType,
                @Description,
                @Floor,
                @Surface
            )
        """
    |> Sql.parameters (paramsFor validated)
    |> Sql.writeAsync
    |> Async.Ignore

let updateLot (connectionString: string) (validated: ValidatedLot) =
    Sql.connect connectionString
    |> Sql.query
        """
            UPDATE Lots SET
                CurrentOwnerPersonId = @CurrentOwnerPersonId,
                CurrentOwnerOrganizationId = @CurrentOwnerOrganizationId,
                Code = @Code,
                LotType = @LotType,
                Description = @Description,
                Floor = @Floor,
                Surface = @Surface
            WHERE LotId = @LotId
        """
    |> Sql.parameters (paramsFor validated)
    |> Sql.writeAsync

let deleteLot connectionString (buildingId, lotId) =
    Sql.connect connectionString
    |> Sql.query
        """
            UPDATE Lots 
            SET IsActive = FALSE
            WHERE LotId = @LotId
            AND BuildingId = @BuildingId
        """
    |> Sql.parameters [
            "@LotId", Sql.uuid lotId
            "@BuildingId", Sql.uuid buildingId
        ]
    |> Sql.writeAsync    

let makeStorage conn = {
    new ILotStorage with
        member _.CreateLot lot = createLot conn lot
        member _.UpdateLot lot = updateLot conn lot
        member _.DeleteLot (buildingId, lotId) = deleteLot conn (buildingId, lotId)
}