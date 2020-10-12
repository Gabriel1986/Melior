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
        "@Share"                     , Sql.intOrNone (validated.Share |> Option.map (fun s -> s.Value ()))
    ]

let private generateSqlForOwners (lotId: Guid, owners: (LotOwnerId * LotOwnerRole) list) =
    owners 
    |> List.map (fun (ownerId, role) ->
        let personId, orgId =
            match ownerId with
            | (LotOwnerId.OwnerId ownerId) ->
                Some ownerId, None
            | (LotOwnerId.OrganizationId orgId) ->
                None, Some orgId
        """
            INSERT INTO LotOwners
                (LotId, Role, PersonId, OrganizationId) 
            VALUES 
                (@LotId, @Role, @PersonId, @OrganizationId)
        """, [
            "@LotId", Sql.uuid lotId
            "@Role", Sql.string (string role)
            "@PersonId", Sql.uuidOrNone personId
            "@OrganizationId", Sql.uuidOrNone orgId
        ]
    )

let createLot (connectionString: string) (validated: ValidatedLot) =
    Sql.connect connectionString
    |> Sql.writeBatchAsync [
        yield
            """
                INSERT INTO Lots (
                    LotId,
                    BuildingId,
                    Code,
                    LotType,
                    Description,
                    Floor,
                    Share
                ) VALUES (
                    @LotId,
                    @BuildingId,
                    @Code,
                    @LotType,
                    @Description,
                    @Floor,
                    @Share
                )
            """, paramsFor validated
        yield!
            generateSqlForOwners (validated.LotId, validated.Owners)
    ]
    |> Async.Ignore

let updateLot (connectionString: string) (validated: ValidatedLot) =
    Sql.connect connectionString
    |> Sql.writeBatchAsync [
        yield
            """
                UPDATE Lots SET
                    Code = @Code,
                    LotType = @LotType,
                    Description = @Description,
                    Floor = @Floor,
                    Share = @Share
                WHERE LotId = @LotId
            """, (paramsFor validated)
        yield
            "DELETE FROM LotOwners WHERE LotId = @LotId", [ "@LotId", Sql.uuid validated.LotId ]
        yield!
            generateSqlForOwners (validated.LotId, validated.Owners)

    ]
    |> Async.map (List.tryHead >> Option.defaultValue 0)

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