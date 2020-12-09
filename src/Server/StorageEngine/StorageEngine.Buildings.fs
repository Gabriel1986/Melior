module Server.StorageEngine.BuildingStorage

open System
open NodaTime
open Npgsql.FSharp
open Shared.Write
open Server.Addresses.Library
open Server.Library
open Server.PostgreSQL
open Server.Blueprint.Data.Storage
open Server.Blueprint.Data.Financial

let private buildingToSqlProps (validated: ValidatedBuilding) =
    let today = DateTime.Today
    
    let generalMeetingFrom =
        validated.GeneralMeetingPeriod 
        |> Option.map (fun d -> (new LocalDate(today.Year, d.FromMonth, d.FromDay)).ToDateTimeUnspecified())
    
    let generalMeetingUntil =
        validated.GeneralMeetingPeriod
        |> Option.map (fun d -> (new LocalDate(today.Year, d.UntilMonth, d.UntilDay)).ToDateTimeUnspecified())
    
    [
        "@BuildingId"         , Sql.uuid validated.BuildingId
        "@Code"               , Sql.string (string validated.Code)
        "@Name"               , Sql.string (string validated.Name)
        "@Address"            , Sql.jsonb (ValidatedAddress.toJson validated.Address)
        "@OrganizationNumber" , Sql.stringOrNone (validated.OrganizationNumber |> Option.map string)
        "@Remarks"            , Sql.stringOrNone validated.Remarks
        "@GeneralMeetingFrom" , Sql.timestampOrNone generalMeetingFrom
        "@GeneralMeetingUntil", Sql.timestampOrNone  generalMeetingUntil
        "@YearOfConstruction" , Sql.intOrNone (validated.YearOfConstruction |> Option.map (fun x -> x.Value ()))
        "@YearOfDelivery"     , Sql.intOrNone (validated.YearOfDelivery |> Option.map (fun x -> x.Value ()))
        "@BankAccounts"       , Sql.jsonb (validated.BankAccounts |> ValidatedBankAccount.listToJson)
        "@PictureId"          , Sql.uuidOrNone validated.PictureId
        "@SharesTotal"        , Sql.intOrNone (validated.SharesTotal |> Option.map (fun x -> x.Value ()))
    ]

let transformEventToSql (msg: Message<BuildingEvent>) =
    match msg.Payload with
    | BuildingEvent.BuildingEvent event ->
        match event with
        | CUDEvent.Created validated ->
            [
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
                        YearOfDelivery,
                        BankAccounts,
                        PictureId,
                        SharesTotal
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
                        @YearOfDelivery,
                        @BankAccounts,
                        @PictureId,
                        @SharesTotal
                    )
                """, [ buildingToSqlProps validated ]
            ]
            //yield! seedFinancialCategoriesSql
        | CUDEvent.Updated validated ->
            [
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
                        YearOfDelivery = @YearOfDelivery,
                        BankAccounts = @BankAccounts,
                        PictureId = @PictureId,
                        SharesTotal = @SharesTotal
                    WHERE BuildingId = @BuildingId
                """, [ buildingToSqlProps validated ]
            ]
        | CUDEvent.Deleted buildingId ->
            [
                """
                    UPDATE Buildings 
                    SET IsActive = FALSE
                    WHERE BuildingId = @BuildingId
                """, [ [ "@BuildingId", Sql.uuid buildingId ] ]
            ]
    | BuildingEvent.ConciergeWasUpdated (buildingId, conciergeId) ->
        let conciergeOwnerId, conciergePerson =
            match conciergeId with
            | Some (ValidatedConcierge.OwnerId ownerId) -> Some ownerId, None
            | Some (ValidatedConcierge.NonOwner person) -> None, Some person
            | None -> None, None
        
        match conciergePerson with
        | Some person ->
            [
                yield! PersonStorage.transformEventToSql (PersonEvent.PersonWasSaved person |> inMsg msg)

                """
                    UPDATE Buildings SET
                        ConciergeOwnerId = @ConciergeOwnerId,
                        ConciergePersonId = @ConciergePersonId
                    WHERE BuildingId = @BuildingId
                """, [ [
                    "@BuildingId", Sql.uuid buildingId
                    "@ConciergeOwnerId" , Sql.uuidOrNone None
                    "@ConciergePersonId", Sql.uuid person.PersonId
                ] ]
            ]
        | None ->
             [
                """
                    UPDATE Buildings SET
                        ConciergeOwnerId = @ConciergeOwnerId,
                        ConciergePersonId = @ConciergePersonId
                    WHERE BuildingId = @BuildingId
                """, [ [
                    "@BuildingId"                , Sql.uuid buildingId
                    "@ConciergeOwnerId"          , Sql.uuidOrNone conciergeOwnerId
                    "@ConciergePersonId"         , Sql.uuidOrNone None
                ] ]
            ]
    | BuildingEvent.SyndicWasUpdated (buildingId, syndicId) ->
        let syndicOwnerId, syndicProfessionalSyndicId, syndicPerson = 
            match syndicId with
            | Some (ValidatedSyndic.OwnerId ownerId) ->
                Some ownerId, None, None
            | Some (ValidatedSyndic.ProfessionalSyndicId proId) ->
                None, Some proId, None
            | Some (ValidatedSyndic.Other person) ->
                None, None , Some person
            | None ->
                None, None , None
        
        match syndicPerson with
        | Some person ->
            [
                yield! PersonStorage.transformEventToSql (PersonEvent.PersonWasSaved person |> inMsg msg)

                """
                    UPDATE Buildings SET
                        SyndicOwnerId = @SyndicOwnerId,
                        SyndicProfessionalSyndicId = @SyndicProfessionalSyndicId
                    WHERE BuildingId = @BuildingId
                """, [ [
                    "@BuildingId"                , Sql.uuid buildingId
                    "@SyndicOwnerId"             , Sql.uuidOrNone None
                    "@SyndicProfessionalSyndicId", Sql.uuidOrNone None
                    "@SyndicPersonId"            , Sql.uuid person.PersonId
                ] ]
            ]
        | None ->
            [
                """
                    UPDATE Buildings SET
                        SyndicOwnerId = @SyndicOwnerId,
                        SyndicProfessionalSyndicId = @SyndicProfessionalSyndicId
                    WHERE BuildingId = @BuildingId
                """, [ [
                    "@BuildingId"                , Sql.uuid buildingId
                    "@SyndicOwnerId"             , Sql.uuidOrNone syndicOwnerId
                    "@SyndicProfessionalSyndicId", Sql.uuidOrNone syndicProfessionalSyndicId
                    "@SyndicPersonId"            , Sql.uuidOrNone None
                ] ]
            ]
