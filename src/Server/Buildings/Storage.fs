module Server.Buildings.Storage

open System
open Npgsql.FSharp
open Server.Addresses.Workflow
open Server.PostgreSQL
open Shared.Write
open NodaTime

let private mapConciergeId =
    function
    | Some (ConciergeId.OwnerId ownerId)     -> Some ownerId, None
    | Some (ConciergeId.NonOwnerId personId) -> None, Some personId
    | None                                   -> None, None

let private mapSyndicId =
    function
    | Some (SyndicId.OwnerId ownerId)            -> Some ownerId, None, None
    | Some (SyndicId.ProfessionalSyndicId proId) -> None, Some proId, None
    | Some (SyndicId.OtherId personId)           -> None, None , Some personId
    | None                                       -> None, None , None


let createBuilding (connectionString: string) (validated: ValidatedBuilding) =
    let today = DateTime.Today
    let conciergeOwnerId, conciergePersonId = mapConciergeId validated.ConciergeId
    let syndicOwnerId, syndicProfessionalSyndicId, syndicPersonId = mapSyndicId validated.SyndicId

    let generalMeetingFrom =
        validated.GeneralMeetingPeriod 
        |> Option.map (fun d -> (new LocalDate(today.Year, d.FromMonth, d.FromDay)).ToDateTimeUnspecified())

    let generalMeetingUntil =
        validated.GeneralMeetingPeriod
        |> Option.map (fun d -> (new LocalDate(today.Year, d.UntilMonth, d.UntilDay)).ToDateTimeUnspecified())

    Sql.connect connectionString
    |> Sql.query
        """
            INSERT INTO Buildings (
                BuildingId,
                IsActive,
                Code,
                Name,
                Address,
                OrganizationNumber,
                Remarks,
                GeneralMeetingFrom,
                GeneralMeetingUntil,
                ConciergeOwnerId,
                ConciergePersonId,
                SyndicOwnerId,
                SyndicProfessionalSyndicId,
                SyndicPersonId,
                YearOfConstruction,
                YearOfDelivery
            ) VALUES (
                @BuildingId,
                @IsActive,
                @Code,
                @Name,
                @Address,
                @OrganizationNumber,
                @Remarks,
                @GeneralMeetingFrom,
                @GeneralMeetingUntil,
                @ConciergeOwnerId,
                @ConciergePersonId,
                @SyndicOwnerId,
                @SyndicProfessionalSyndicId,
                @SyndicPersonId,
                @YearOfConstruction,
                @YearOfDelivery
            )
        """
    |> Sql.parameters [
            "@BuildingId"                , Sql.uuid validated.BuildingId
            "@IsActive"                  , Sql.bool true
            "@Code"                      , Sql.string (string validated.Code)
            "@Name"                      , Sql.string (string validated.Name)
            "@Address"                   , Sql.jsonb (ValidatedAddress.toJson validated.Address)
            "@OrganizationNumber"        , Sql.stringOrNone (validated.OrganizationNumber |> Option.map string)
            "@Remarks"                   , Sql.stringOrNone validated.Remarks
            "@GeneralMeetingFrom"        , Sql.timestampOrNone generalMeetingFrom
            "@GeneralMeetingUntil"       , Sql.timestampOrNone  generalMeetingUntil
            "@ConciergeOwnerId"          , Sql.uuidOrNone conciergeOwnerId
            "@ConciergePersonId"         , Sql.uuidOrNone conciergePersonId
            "@SyndicOwnerId"             , Sql.uuidOrNone syndicOwnerId
            "@SyndicProfessionalSyndicId", Sql.uuidOrNone syndicProfessionalSyndicId
            "@SyndicPersonId"            , Sql.uuidOrNone syndicPersonId
            "@YearOfConstruction"        , Sql.intOrNone (validated.YearOfConstruction |> Option.map (fun x -> x.Value))
            "@YearOfDelivery"            , Sql.intOrNone (validated.YearOfDelivery |> Option.map (fun x -> x.Value))
        ]
    |> Sql.writeAsync

let updateBuilding (connectionString: string) (validated: ValidatedBuilding) =
    let today = DateTime.Today
    let conciergeOwnerId, conciergePersonId = mapConciergeId validated.ConciergeId
    let syndicOwnerId, syndicProfessionalSyndicId, syndicPersonId = mapSyndicId validated.SyndicId

    let generalMeetingFrom =
        validated.GeneralMeetingPeriod 
        |> Option.map (fun d -> (new LocalDate(today.Year, d.FromMonth, d.FromDay)).ToDateTimeUnspecified())

    let generalMeetingUntil =
        validated.GeneralMeetingPeriod
        |> Option.map (fun d -> (new LocalDate(today.Year, d.UntilMonth, d.UntilDay)).ToDateTimeUnspecified())

    Sql.connect connectionString
    |> Sql.query
        """
            UPDATE Buildings SET
                BuildingId = @BuildingId,
                IsActive = @IsActive,
                Code = @Code,
                Name = @Name,
                Address = @Address,
                OrganizationNumber = @OrganizationNumber,
                Remarks = @Remarks,
                GeneralMeetingFrom = @GeneralMeetingFrom,
                GeneralMeetingUntil= @GeneralMeetingUntil,
                ConciergeOwnerId = @ConciergeOwnerId,
                ConciergePersonId = @ConciergePersonId,
                SyndicOwnerId = @SyndicOwnerId,
                SyndicProfessionalSyndicId = @SyndicProfessionalSyndicId,
                SyndicPersonId = @SyndicPersonId,
                YearOfConstruction = @YearOfConstruction,
                YearOfDelivery = @YearOfDelivery
            WHERE BuildingId = @BuildingId
        """
    |> Sql.parameters [
            "@BuildingId"                , Sql.uuid validated.BuildingId
            "@IsActive"                  , Sql.bool true
            "@Code"                      , Sql.string (string validated.Code)
            "@Name"                      , Sql.string (string validated.Name)
            "@Address"                   , Sql.jsonb (ValidatedAddress.toJson validated.Address)
            "@OrganizationNumber"        , Sql.stringOrNone (validated.OrganizationNumber |> Option.map string)
            "@Remarks"                   , Sql.stringOrNone validated.Remarks
            "@GeneralMeetingFrom"        , Sql.timestampOrNone generalMeetingFrom
            "@GeneralMeetingUntil"       , Sql.timestampOrNone  generalMeetingUntil
            "@ConciergeOwnerId"          , Sql.uuidOrNone conciergeOwnerId
            "@ConciergePersonId"         , Sql.uuidOrNone conciergePersonId
            "@SyndicOwnerId"             , Sql.uuidOrNone syndicOwnerId
            "@SyndicProfessionalSyndicId", Sql.uuidOrNone syndicProfessionalSyndicId
            "@SyndicPersonId"            , Sql.uuidOrNone syndicPersonId
            "@YearOfConstruction"        , Sql.intOrNone (validated.YearOfConstruction |> Option.map (fun x -> x.Value))
            "@YearOfDelivery"            , Sql.intOrNone (validated.YearOfDelivery |> Option.map (fun x -> x.Value))
        ]
    |> Sql.writeAsync