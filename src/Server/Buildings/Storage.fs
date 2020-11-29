module Server.Buildings.Storage

open System
open NodaTime
open Npgsql.FSharp
open Server.Addresses.Workflow
open Server.PostgreSQL
open Server.Library
open Server.SeedData
open Shared.Read
open Shared.Write

type IBuildingStorage =
    abstract CreateBuilding: ValidatedBuilding -> Async<unit>
    abstract UpdateBuilding: ValidatedBuilding -> Async<int>
    abstract DeleteBuilding: BuildingId -> Async<int>
    abstract UpdateBuildingSyndic: BuildingId * ValidatedSyndicInput option -> Async<int>
    abstract UpdateBuildingConcierge: BuildingId * ValidatedConciergeInput option -> Async<int>
and ValidatedSyndicInput =
    | ProfessionalSyndicId of Guid
    | OwnerId of Guid
    | Other of ValidatedPerson
    static member Validate =
        function
        | SyndicInput.ProfessionalSyndicId proId -> Ok (ValidatedSyndicInput.ProfessionalSyndicId proId)
        | SyndicInput.OwnerId ownerId -> Ok (ValidatedSyndicInput.OwnerId ownerId)
        | SyndicInput.Other person ->
            ValidatedPerson.Validate person
            |> Result.map ValidatedSyndicInput.Other
and ValidatedConciergeInput =
    | OwnerId of Guid
    | NonOwner of ValidatedPerson
    static member Validate =
        function
        | ConciergeInput.OwnerId ownerId -> Ok (ValidatedConciergeInput.OwnerId ownerId)
        | ConciergeInput.NonOwner person ->
            ValidatedPerson.Validate person
            |> Result.map ValidatedConciergeInput.NonOwner

let private paramsFor (validated: ValidatedBuilding) =
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

let updateBuildingSyndic (connectionString: string) (buildingId: BuildingId, syndicId: ValidatedSyndicInput option) =
    let syndicOwnerId, syndicProfessionalSyndicId, syndicPerson = 
        match syndicId with
        | Some (ValidatedSyndicInput.OwnerId ownerId)            -> Some ownerId, None, None
        | Some (ValidatedSyndicInput.ProfessionalSyndicId proId) -> None, Some proId, None
        | Some (ValidatedSyndicInput.Other person)           -> None, None , Some person
        | None                                       -> None, None , None

    match syndicPerson with
    | Some person ->
        Sql.connect connectionString
        |> Sql.writeBatchAsync [
            Server.Persons.Storage.upsertQuery,
                Server.Persons.Storage.paramsFor person
        
            """
                UPDATE Buildings SET
                    SyndicOwnerId = @SyndicOwnerId,
                    SyndicProfessionalSyndicId = @SyndicProfessionalSyndicId
                WHERE BuildingId = @BuildingId
            """, [
                "@BuildingId"                , Sql.uuid buildingId
                "@SyndicOwnerId"             , Sql.uuidOrNone None
                "@SyndicProfessionalSyndicId", Sql.uuidOrNone None
                "@SyndicPersonId"            , Sql.uuid person.PersonId
            ]
        ]
        |> Async.map (List.skip 1 >> List.tryHead >> Option.defaultValue 0)
    | None ->
        Sql.connect connectionString
        |> Sql.query
            """
                UPDATE Buildings SET
                    SyndicOwnerId = @SyndicOwnerId,
                    SyndicProfessionalSyndicId = @SyndicProfessionalSyndicId
                WHERE BuildingId = @BuildingId
            """
        |> Sql.parameters [
            "@BuildingId"                , Sql.uuid buildingId
            "@SyndicOwnerId"             , Sql.uuidOrNone syndicOwnerId
            "@SyndicProfessionalSyndicId", Sql.uuidOrNone syndicProfessionalSyndicId
            "@SyndicPersonId"            , Sql.uuidOrNone None
        ]
        |> Sql.writeAsync

let updateBuildingConcierge (connectionString: string) (buildingId: BuildingId, conciergeId: ValidatedConciergeInput option) =
    let conciergeOwnerId, conciergePerson =
        match conciergeId with
        | Some (ValidatedConciergeInput.OwnerId ownerId)     -> Some ownerId, None
        | Some (ValidatedConciergeInput.NonOwner person) -> None, Some person
        | None -> None, None

    match conciergePerson with
    | Some person ->
        Sql.connect connectionString
        |> Sql.writeBatchAsync [
            Server.Persons.Storage.upsertQuery,
                Server.Persons.Storage.paramsFor person
        
            """
                UPDATE Buildings SET
                    ConciergeOwnerId = @ConciergeOwnerId,
                    ConciergePersonId = @ConciergePersonId
                WHERE BuildingId = @BuildingId
            """, [
                "@BuildingId"                , Sql.uuid buildingId
                "@ConciergeOwnerId"          , Sql.uuidOrNone None
                "@ConciergePersonId"         , Sql.uuid person.PersonId
            ]
        ]
        |> Async.map (List.skip 1 >> List.tryHead >> Option.defaultValue 0)
    | None ->
        Sql.connect connectionString
        |> Sql.query
            """
                UPDATE Buildings SET
                    ConciergeOwnerId = @ConciergeOwnerId,
                    ConciergePersonId = @ConciergePersonId
                WHERE BuildingId = @BuildingId
            """
        |> Sql.parameters [
            "@BuildingId"                , Sql.uuid buildingId
            "@ConciergeOwnerId"          , Sql.uuidOrNone conciergeOwnerId
            "@ConciergePersonId"         , Sql.uuidOrNone None
        ]
        |> Sql.writeAsync

let private seedFinancialCategoriesForBuildingSql (buildingId: BuildingId) = async {
    let! financialCategories = FinancialCategories.readPredefined ()
    return
        financialCategories
        |> Seq.map (fun row ->
            """
                INSERT INTO FinancialCategories (
                    FinancialCategoryId,
                    BuildingId,
                    Code,
                    Description
                ) VALUES (
                    uuid_generate_v4(), @BuildingId, @Code, @Description
                )
            """, [
                "@BuildingId", Sql.uuid buildingId
                "@Code", Sql.string row.Code
                "@Description", Sql.string row.Description
            ])
}

let createBuilding (connectionString: string) (validated: ValidatedBuilding) = async {
    let! seedFinancialCategoriesSql = seedFinancialCategoriesForBuildingSql validated.BuildingId

    do!
        Sql.connect connectionString
        |> Sql.writeBatchAsync [
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
            """, (paramsFor validated)

            yield! seedFinancialCategoriesSql
        ]
        |> Async.Ignore
}

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
                YearOfDelivery = @YearOfDelivery,
                BankAccounts = @BankAccounts,
                PictureId = @PictureId,
                SharesTotal = @SharesTotal
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

let makeStorage conn = {
    new IBuildingStorage with
        member _.CreateBuilding building = createBuilding conn building     
        member _.UpdateBuilding building = updateBuilding conn building
        member _.DeleteBuilding buildingId = deleteBuilding conn buildingId     
        member _.UpdateBuildingSyndic (buildingId, syndicId) = updateBuildingSyndic conn (buildingId, syndicId)
        member _.UpdateBuildingConcierge (buildingId, conciergeId) = updateBuildingConcierge conn (buildingId, conciergeId)
}