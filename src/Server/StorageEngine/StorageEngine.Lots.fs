module Server.StorageEngine.LotStorage

open System
open Npgsql.FSharp
open Shared.Read
open Shared.Write
open Server.Library
open Server.PostgreSQL
open Server.Blueprint.Data.Storage

let private lotToSqlProps (validated: ValidatedLot) = [
    "@LotId", Sql.uuid validated.LotId
    "@BuildingId", Sql.uuid validated.BuildingId
    "@Code", Sql.string (string validated.Code)
    "@LotType", Sql.string (string validated.LotType)
    "@Description", Sql.stringOrNone validated.Description
    "@Floor", Sql.intOrNone validated.Floor
    "@Share", Sql.intOrNone (validated.Share |> Option.map (fun s -> s.Value ()))
]

let private paramsForLotOwner (validated: ValidatedLotOwner) =
    let personId, orgId =
        match validated.LotOwnerType with
        | (LotOwnerType.Owner owner) ->
            Some owner.PersonId, None
        | (LotOwnerType.Organization org) ->
            None, Some org.OrganizationId
    [
        "@LotId", Sql.uuid validated.LotId
        "@BuildingId", Sql.uuid validated.BuildingId
        "@LotOwnerId", Sql.uuid validated.LotOwnerId
        "@PersonId", Sql.uuidOrNone personId
        "@OrganizationId", Sql.uuidOrNone orgId
        "@StartDate", Sql.timestamp (validated.StartDate.AddHours(2.0).Date)
        "@EndDate", Sql.timestampOrNone (validated.EndDate |> Option.map (fun dt -> (dt.AddHours(2.0).Date)))
    ]

let private setLotOwnerContactsFor (msg: Message<ValidatedLotOwner>) =
    let validated = msg.Payload
    let persons = validated.Contacts |> List.choose (function | ValidatedLotOwnerContact.NonOwner person -> Some person | _ -> None)
    
    [
        "DELETE FROM LotOwnerContacts WHERE LotOwnerId = @LotOwnerId"
        , [[ "@LotOwnerId", Sql.uuid validated.LotOwnerId ]]

        yield! 
            persons |> List.collect (PersonEvent.PersonWasSaved >> inMsg msg >> PersonStorage.transformEventToSql)
    
        """
            INSERT INTO LotOwnerContacts (LotOwnerId, BuildingId, ContactOwnerId, ContactPersonId) 
            VALUES (@LotOwnerId, @BuildingId, @ContactOwnerId, @ContactPersonId)
        """
        , validated.Contacts |> List.map (fun contact -> 
            let ownerId, buildingId, personId =
                match contact with
                | ValidatedLotOwnerContact.Owner o -> Some o.PersonId, Some o.BuildingId, None
                | ValidatedLotOwnerContact.NonOwner p -> None, None, Some p.PersonId
    
            [
                "@LotOwnerId", Sql.uuid validated.LotOwnerId
                "@BuildingId", Sql.uuidOrNone buildingId
                "@ContactOwnerId", Sql.uuidOrNone ownerId
                "@ContactPersonId", Sql.uuidOrNone personId
            ])
    ]

let transformEventToSql (msg: Message<LotEvent>) =
    match msg.Payload with
    | LotEvent.LotEvent event ->
        match event with
        | BuildingSpecificCUDEvent.Created validated ->
            [
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
                """, [ lotToSqlProps validated ]
            ]
        | BuildingSpecificCUDEvent.Updated validated ->
            [
                """
                    UPDATE Lots SET
                        Code = @Code,
                        LotType = @LotType,
                        Description = @Description,
                        Floor = @Floor,
                        Share = @Share
                    WHERE LotId = @LotId
                """, [ lotToSqlProps validated ]
            ]
        | BuildingSpecificCUDEvent.Deleted (buildingId: BuildingId, lotId: Guid) ->
            [
                """
                    UPDATE Lots 
                    SET IsActive = FALSE
                    WHERE LotId = @LotId
                    AND BuildingId = @BuildingId
                """, [ [
                    "@LotId", Sql.uuid lotId
                    "@BuildingId", Sql.uuid buildingId
                ] ]
            ]
    | LotOwnerEvent e ->
        match e with
        | BuildingSpecificCUDEvent.Created (_, validated) ->
            [
                """
                    INSERT INTO LotOwners
                        (LotId, BuildingId, LotOwnerId, Role, PersonId, OrganizationId, StartDate, EndDate, OGMReference)
                    VALUES
                        (@LotId, @BuildingId, @LotOwnerId, @Role, @PersonId, @OrganizationId, @StartDate, @EndDate, @OGMReference)
                """, [
                    paramsForLotOwner validated @ [
                        "@OGMReference", Sql.string ((Shared.ConstrainedTypes.BelgianOGM.Generate ()).Value ())
                    ]
                ]

                yield! setLotOwnerContactsFor (validated |> inMsg msg)
            ]
        | BuildingSpecificCUDEvent.Updated (_, validated) ->
            [
                "UPDATE LotOwners SET StartDate = @StartDate, EndDate = @EndDate WHERE LotOwnerId = @LotOwnerId AND BuildingId = @BuildingId"
                , [ paramsForLotOwner validated ]

                yield! setLotOwnerContactsFor (validated |> inMsg msg)
            ]
        | BuildingSpecificCUDEvent.Deleted (buildingId, lotOwnerId) ->
            [
                "UPDATE LotOwners SET ISDELETED = TRUE WHERE LotOwnerId = @LotOwnerId AND BuildingId = @BuildingId"
                , [[ 
                    "@LotOwnerId", Sql.uuid lotOwnerId
                    "@BuildingId", Sql.uuid buildingId
                ]]
            ]
    | LotOwnerOGMReferenceWasUpdated (lotOwnerId, newOgm) ->
        [
            """
                UPDATE LotOwners SET OGMReference = @OGMReference WHERE LotOwnerId = @LotOwnerId
            """
            , [[
                "@LotOwnerId", Sql.uuid lotOwnerId
                "@OGMReference", Sql.string (newOgm.Value ())
            ]]
        ]