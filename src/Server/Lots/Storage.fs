module Server.Lots.Storage

open Npgsql.FSharp
open Server.PostgreSQL
open Shared.Write

let private paramsFor (validated: ValidatedLot) =
    let lotOwnerPersonId = validated.CurrentOwnerId |> Option.bind (function | LotOwnerId.OwnerId ownerId -> Some ownerId | _ -> None)
    let lotOwnerOrganizationId = validated.CurrentOwnerId |> Option.bind (function | LotOwnerId.OrganizationId orgId -> Some orgId | _ -> None)
    
    [
        "@LotId"                     , Sql.uuid validated.LotId
        "@BuildingId"                , Sql.uuid validated.BuildingId
        "@CurrentOwnerPersonId"      , Sql.uuidOrNone lotOwnerPersonId
        "@CurrentOwnerOrganizationId", Sql.uuidOrNone lotOwnerOrganizationId
        "@Code"                      , Sql.string (string validated.Code)
        "@LotType"                   , Sql.string (string validated.LotType)
        "@Description"               , Sql.stringOrNone validated.Description
        "@Floor"                     , Sql.intOrNone validated.Floor
        "@Surface"                   , Sql.intOrNone (validated.Surface |> Option.map (fun s -> s.Value))
    ]

let createLot (connectionString: string) (validated: ValidatedLot) =
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

let deleteLot connectionString lotId =
    Sql.connect connectionString
    |> Sql.query
        """
            UPDATE Lots 
            SET IsActive = FALSE
            WHERE LotId = @LotId
        """
    |> Sql.parameters [
            "@LotId", Sql.uuid lotId
        ]
    |> Sql.writeAsync    