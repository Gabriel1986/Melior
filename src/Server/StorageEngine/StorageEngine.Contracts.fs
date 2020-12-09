module Server.StorageEngine.ContractStorage

open System
open Npgsql.FSharp
open Shared.Read
open Shared.Write
open Server.Library
open Server.LibraryExtensions
open Server.PostgreSQL
open Server.Blueprint.Data.Storage
open Server.Blueprint.Data.Contracts

let private toDBContractType (validated: ValidatedContractContractType): DbContractType =
    match validated with
    | ValidatedOtherContractType name -> OtherContractType (string name)
    | ValidatedInsuranceContractType validated -> InsuranceContractType {| Name = string validated.Name; BrokerId = validated.BrokerId |}
    | ValidatedPredefinedContractType predefined -> PredefinedContractType predefined

let private contractToSqlProps (msg: Message<ValidatedContract>) =
    let validated = msg.Payload
    let currentUser = msg.CurrentUser
    [
        "@ContractId", Sql.uuid validated.ContractId
        "@BuildingId", Sql.uuid validated.BuildingId
        "@ContractOrganizationId", Sql.uuidOrNone validated.ContractOrganizationId
        "@ContractType", Sql.string (Thoth.Json.Net.Encode.Auto.toString(0, toDBContractType validated.ContractType))
        "@CreatedBy", Sql.string (currentUser.Principal ())
        "@CreatedAt", Sql.timestamp DateTime.UtcNow
        "@LastUpdatedBy", Sql.string (currentUser.Principal ())
        "@LastUpdatedAt", Sql.timestamp DateTime.UtcNow
    ]

let transformEventToSql (msg: Message<ContractEvent>) =
    match msg.Payload with
    | ContractEvent.ContractEvent event ->
        match event with
        | BuildingSpecificCUDEvent.Created validated ->
            [
                """
                    INSERT INTO Contracts 
                        (ContractId, BuildingId, ContractOrganizationId, ContractType, CreatedBy, CreatedAt, LastUpdatedBy, LastUpdatedAt)
                    VALUES
                        (@ContractId, @BuildingId, @ContractOrganizationId, @ContractType, @CreatedBy, @CreatedAt, @LastUpdatedBy, @LastUpdatedAt)
                """
                , [ validated |> inMsg msg |> contractToSqlProps ]
            ]
        | BuildingSpecificCUDEvent.Updated validated ->
            [
                """
                    UPDATE Contracts
                        SET 
                            ContractOrganizationId = @ContractOrganizationId,
                            ContractType = @ContractType,
                            LastUpdatedBy = @LastUpdatedBy,
                            LastUpdatedAt = @LastUpdatedAt
                        WHERE
                            ContractId = @ContractId AND BuildingId = @BuildingId;
                """
                , [ validated |> inMsg msg |> contractToSqlProps ]
            ]
        | BuildingSpecificCUDEvent.Deleted (buildingId: BuildingId, contractId: Guid) ->
            [
                """
                    UPDATE Contracts
                        SET 
                            IsActive = FALSE, LastUpdatedBy = @LastUpdatedBy, LastUpdatedAt = @LastUpdatedAt
                        WHERE
                            ContractId = @ContractId AND BuildingId = @BuildingId;
                """, [[ 
                    "@ContractId", Sql.uuid contractId
                    "@BuildingId", Sql.uuid buildingId
                    "@LastUpdatedBy", Sql.string (msg.CurrentUser.Principal ())
                    "@LastUpdatedAt", Sql.timestamp DateTime.UtcNow
                ]]
            ]
    | ContractEvent.ContractTypeAnswersWereUpdated contractTypeAnswers ->
        [
            """
                INSERT INTO ContractTypeAnswers (BuildingId, Question, IsTrue) VALUES (@BuildingId, @Question, @IsTrue)
                ON CONFLICT ON CONSTRAINT contracttypeanswers_pkey DO UPDATE SET IsTrue = @IsTrue
            """
            , contractTypeAnswers |> List.map (fun answer ->
                [
                    "@BuildingId", Sql.uuid answer.BuildingId
                    "@Question", Sql.string (string answer.Question)
                    "@IsTrue", Sql.bool answer.IsTrue
                ]
            )
        ]