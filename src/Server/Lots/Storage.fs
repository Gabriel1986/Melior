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
        "@LotId", Sql.uuid validated.LotId
        "@BuildingId", Sql.uuid validated.BuildingId
        "@Code", Sql.string (string validated.Code)
        "@LotType", Sql.string (string validated.LotType)
        "@Description", Sql.stringOrNone validated.Description
        "@Floor", Sql.intOrNone validated.Floor
        "@Share", Sql.intOrNone (validated.Share |> Option.map (fun s -> s.Value ()))
    ]

let private generateInsertSqlForOwners (owners: ValidatedLotOwner list) =
    owners 
    |> List.map (fun owner ->
        let personId, orgId =
            match owner.LotOwnerTypeId with
            | (LotOwnerTypeId.OwnerId ownerId) ->
                Some ownerId, None
            | (LotOwnerTypeId.OrganizationId orgId) ->
                None, Some orgId
        """
            INSERT INTO LotOwners
                (LotId, LotOwnerId, Role, PersonId, OrganizationId, StartDate, EndDate)
            VALUES 
                (@LotId, @LotOwnerId, @Role, @PersonId, @OrganizationId, @StartDate, @EndDate)
        """, [
            "@LotId", Sql.uuid owner.LotId
            "@LotOwnerId", Sql.uuid owner.LotOwnerId
            "@Role", Sql.string (string owner.LotOwnerRole)
            "@PersonId", Sql.uuidOrNone personId
            "@OrganizationId", Sql.uuidOrNone orgId
            "@StartDate", Sql.timestamp owner.StartDate.Date
            "@EndDate", Sql.timestampOrNone (owner.EndDate |> Option.map (fun dt -> dt.Date))
        ]
    )

let private generateDeleteSqlForOwners (deletedOwners: Guid list) = [
    yield!
        deletedOwners
        |> List.ofSeq
        |> List.map (fun deleted ->
            "UPDATE LotOwners SET ISDELETED = TRUE WHERE LotOwnerId = @LotOwnerId", [
                "@LotOwnerId", Sql.uuid deleted
            ]
        )
]

let private generateUpdateSqlForOwners (owners: ValidatedLotOwner list) =
    owners
    |> List.map (fun owner ->
        """
            UPDATE LotOwners SET Role = @Role, StartDate = @StartDate, EndDate = @EndDate 
            WHERE LotOwnerId = @LotOwnerId
        """
        , [
            yield "@Role", Sql.string (string owner.LotOwnerRole)
            yield "@LotId", Sql.uuid owner.LotId
            yield "@StartDate", Sql.timestamp (owner.StartDate.AddHours(2.0).Date)
            yield "@EndDate", Sql.timestampOrNone (owner.EndDate |> Option.map (fun dt -> dt.AddHours(2.0).Date))
            yield "@LotOwnerId", Sql.uuid owner.LotOwnerId
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
            generateInsertSqlForOwners validated.Owners
    ]
    |> Async.Ignore

let updateLot (connectionString: string) (validated: ValidatedLot) = async {
    let! currentLotOwners = 
        Sql.connect connectionString
        |> Sql.query "SELECT LotOwnerId FROM LotOwners WHERE LotId = @LotId"
        |> Sql.parameters [ "@LotId", Sql.uuid validated.LotId ]
        |> Sql.read (fun reader -> reader.uuid "LotOwnerId")

    let currentLotOwnersSet = 
        currentLotOwners 
        |> Set.ofList
    let updatedLotOwnersSet = 
        validated.Owners 
        |> List.map (fun owner -> owner.LotOwnerId) 
        |> Set.ofList

    let updatedOwners, newOwners =
        validated.Owners |> List.partition (fun updated -> currentLotOwnersSet.Contains(updated.LotOwnerId))
    let deletedOwners =
        currentLotOwners |> List.filter (updatedLotOwnersSet.Contains >> not)

    return!
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
            yield!
                generateDeleteSqlForOwners deletedOwners
            yield!
                generateUpdateSqlForOwners updatedOwners
            yield!
                generateInsertSqlForOwners newOwners
        ]
        |> Async.map (List.tryHead >> Option.defaultValue 0)
}

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