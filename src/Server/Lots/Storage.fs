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
            match owner.LotOwnerId with
            | (LotOwnerId.OwnerId ownerId) ->
                Some ownerId, None
            | (LotOwnerId.OrganizationId orgId) ->
                None, Some orgId
        """
            INSERT INTO LotOwners
                (LotId, Role, PersonId, OrganizationId, StartDate, EndDate)
            VALUES 
                (@LotId, @Role, @PersonId, @OrganizationId, @StartDate, @EndDate)
        """, [
            "@LotId", Sql.uuid owner.LotId
            "@Role", Sql.string (string owner.LotOwnerRole)
            "@PersonId", Sql.uuidOrNone personId
            "@OrganizationId", Sql.uuidOrNone orgId
            "@StartDate", Sql.timestamp owner.StartDate.Date
            "@EndDate", Sql.timestampOrNone (owner.EndDate |> Option.map (fun dt -> dt.Date))
        ]
    )

let private generateDeleteSqlForOwners (lotId: Guid, deletedOwners: LotOwnerId list) = [
    yield!
        deletedOwners
        |> List.ofSeq
        |> List.map (fun deleted ->
            match deleted with
            | LotOwnerId.OrganizationId orgId ->
                "DELETE FROM LotOwners WHERE LotId = @LotId AND OrganizationId = @OrganizationId", [
                    "@LotId", Sql.uuid lotId
                    "@OrganizationId", Sql.uuid orgId
                ]
            | LotOwnerId.OwnerId personId ->
                "DELETE FROM LotOwners WHERE LotId = @LotId AND PersonId = @PersonId", [
                    "@LotId", Sql.uuid lotId
                    "@PersonId", Sql.uuid personId
                ]
        )
]

let private generateUpdateSqlForOwners (owners: ValidatedLotOwner list) =
    owners
    |> List.map (fun owner ->
        let query, queryParameters =
            match owner.LotOwnerId with
            | LotOwnerId.OwnerId ownerId ->
                "PersonId = @PersonId", [ "@PersonId", Sql.uuid ownerId ]
            | LotOwnerId.OrganizationId orgId ->
                "OrganizationId = @OrganizationId", [ "@OrganizationId", Sql.uuid orgId ]

        sprintf
            """
                UPDATE LotOwners SET Role = @Role, StartDate = @StartDate, EndDate = @EndDate 
                WHERE LotId = @LotId 
                AND %s
            """
            query
        , [ 
            yield "@Role", Sql.string (string owner.LotOwnerRole)
            yield "@LotId", Sql.uuid owner.LotId
            yield "@StartDate", Sql.timestamp (owner.StartDate.AddHours(2.0).Date)
            yield "@EndDate", Sql.timestampOrNone (owner.EndDate |> Option.map (fun dt -> dt.AddHours(2.0).Date))
            yield! queryParameters
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
        |> Sql.query "SELECT PersonId, OrganizationId FROM LotOwners WHERE LotId = @LotId"
        |> Sql.parameters [ "@LotId", Sql.uuid validated.LotId ]
        |> Sql.read (fun reader ->
            match (reader.uuidOrNone "PersonId", reader.uuidOrNone "OrganizationId") with
            | Some personId, _ -> LotOwnerId.OwnerId personId
            | _, Some organizationId -> LotOwnerId.OrganizationId organizationId
            | _ -> failwithf "Precondition failed... PersonId or OrganizationId should be filled in.")

    let newOwners = 
        validated.Owners 
        |> List.filter (fun updated -> 
            currentLotOwners |> List.contains(updated.LotOwnerId) |> not
        )
    let updatedOwners = 
        validated.Owners 
        |> List.filter (fun updated -> 
            currentLotOwners |> List.contains(updated.LotOwnerId)
        )
    let deletedOwners =
        currentLotOwners
        |> List.filter (fun current ->
            validated.Owners |> List.exists (fun o -> o.LotOwnerId = current) |> not
        )

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
                generateDeleteSqlForOwners (validated.LotId, deletedOwners)
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