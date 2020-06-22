module Server.Buildings.Storage

open System
open Npgsql.FSharp
open Server.Addresses.Workflow
open Server.PostgreSQL
open Shared.Write
open NodaTime

let private paramsFor (validated: ValidatedBuilding) =
    let today = DateTime.Today

    let generalMeetingFrom =
        validated.GeneralMeetingPeriod 
        |> Option.map (fun d -> (new LocalDate(today.Year, d.FromMonth, d.FromDay)).ToDateTimeUnspecified())

    let generalMeetingUntil =
        validated.GeneralMeetingPeriod
        |> Option.map (fun d -> (new LocalDate(today.Year, d.UntilMonth, d.UntilDay)).ToDateTimeUnspecified())

    [
        "@BuildingId"                , Sql.uuid validated.BuildingId
        "@Code"                      , Sql.string (string validated.Code)
        "@Name"                      , Sql.string (string validated.Name)
        "@Address"                   , Sql.jsonb (ValidatedAddress.toJson validated.Address)
        "@OrganizationNumber"        , Sql.stringOrNone (validated.OrganizationNumber |> Option.map string)
        "@Remarks"                   , Sql.stringOrNone validated.Remarks
        "@GeneralMeetingFrom"        , Sql.timestampOrNone generalMeetingFrom
        "@GeneralMeetingUntil"       , Sql.timestampOrNone  generalMeetingUntil
        "@YearOfConstruction"        , Sql.intOrNone (validated.YearOfConstruction |> Option.map (fun x -> x.Value))
        "@YearOfDelivery"            , Sql.intOrNone (validated.YearOfDelivery |> Option.map (fun x -> x.Value))
    ]

let updateBuildingSyndic (connectionString: string) (buildingId: Guid, syndicId: SyndicId option) =
    let syndicOwnerId, syndicProfessionalSyndicId, syndicPersonId = 
        match syndicId with
        | Some (SyndicId.OwnerId ownerId)            -> Some ownerId, None, None
        | Some (SyndicId.ProfessionalSyndicId proId) -> None, Some proId, None
        | Some (SyndicId.OtherId personId)           -> None, None , Some personId
        | None                                       -> None, None , None

    Sql.connect connectionString
    |> Sql.query
        """
            UPDATE Buildings SET
                SyndicOwnerId = @SyndicOwnerId,
                SyndicProfessionalSyndicId = @SyndicProfessionalSyndicId,
                SyndicPersonId = @SyndicPersonId,
            WHERE BuildingId = @BuildingId
        """
    |> Sql.parameters [
        "@BuildingId"                , Sql.uuid buildingId
        "@SyndicOwnerId"             , Sql.uuidOrNone syndicOwnerId
        "@SyndicProfessionalSyndicId", Sql.uuidOrNone syndicProfessionalSyndicId
        "@SyndicPersonId"            , Sql.uuidOrNone syndicPersonId
    ]
    |> Sql.writeAsync

let updateBuildingConcierge (connectionString: string) (buildingId: Guid, conciergeId: ConciergeId option) =
    let conciergeOwnerId, conciergePersonId =
        match conciergeId with
        | Some (ConciergeId.OwnerId ownerId)     -> Some ownerId, None
        | Some (ConciergeId.NonOwnerId personId) -> None, Some personId
        | None                                   -> None, None

    Sql.connect connectionString
    |> Sql.query
        """
            UPDATE Buildings SET
                ConciergeOwnerId = @ConciergeOwnerId,
                ConciergePersonId = @ConciergePersonId,
            WHERE BuildingId = @BuildingId
        """
    |> Sql.parameters [
        "@BuildingId"                , Sql.uuid buildingId
        "@ConciergeOwnerId"          , Sql.uuidOrNone conciergeOwnerId
        "@ConciergePersonId"         , Sql.uuidOrNone conciergePersonId
    ]
    |> Sql.writeAsync

let createBuilding (connectionString: string) (validated: ValidatedBuilding) =
    Sql.connect connectionString
    |> Sql.query
        """
            INSERT INTO Buildings (
                BuildingId,
                Code,
                Name,
                Address,
                OrganizationNumber,
                Remarks,
                GeneralMeetingFrom,
                GeneralMeetingUntil,
                YearOfConstruction,
                YearOfDelivery
            ) VALUES (
                @BuildingId,
                @Code,
                @Name,
                @Address,
                @OrganizationNumber,
                @Remarks,
                @GeneralMeetingFrom,
                @GeneralMeetingUntil,
                @YearOfConstruction,
                @YearOfDelivery
            )
        """
    |> Sql.parameters (paramsFor validated)
    |> Sql.writeAsync

let updateBuilding (connectionString: string) (validated: ValidatedBuilding) =
    Sql.connect connectionString
    |> Sql.query
        """
            UPDATE Buildings SET
                Code = @Code,
                Name = @Name,
                Address = @Address,
                OrganizationNumber = @OrganizationNumber,
                Remarks = @Remarks,
                GeneralMeetingFrom = @GeneralMeetingFrom,
                GeneralMeetingUntil= @GeneralMeetingUntil,
                YearOfConstruction = @YearOfConstruction,
                YearOfDelivery = @YearOfDelivery
            WHERE BuildingId = @BuildingId
        """
    |> Sql.parameters (paramsFor validated)
    |> Sql.writeAsync

let deleteBuilding connectionString buildingId =
    Sql.connect connectionString
    |> Sql.query
        """
            UPDATE Buildings 
            SET IsActive = FALSE
            WHERE BuildingId = @BuildingId
        """
    |> Sql.parameters [
            "@BuildingId", Sql.uuid buildingId
        ]
    |> Sql.writeAsync