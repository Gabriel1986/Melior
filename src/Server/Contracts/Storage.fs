module Server.Contracts.Storage

open System
open Shared.Write
open Shared.Read
open Npgsql.FSharp
open Server.PostgreSQL
open Server.Library
open Server.LibraryExtensions

type IContractStorage =
    abstract CreateContract: Message<ValidatedContract> -> Async<unit>
    abstract UpdateContract: Message<ValidatedContract> -> Async<int>
    abstract DeleteContract: Message<BuildingId * Guid> -> Async<int>
    abstract SaveContractTypeAnswer: ContractTypeAnswer -> Async<int>

let toParams (msg: Message<ValidatedContract>) = [
    "@ContractId", Sql.uuid msg.Payload.ContractId
    "@BuildingId", Sql.uuid msg.Payload.BuildingId
    "@ContractFileId", Sql.uuidOrNone msg.Payload.ContractFileId
    "@ContractOrganizationId", Sql.uuidOrNone msg.Payload.ContractOrganizationId
    "@ContractType", Sql.string (Thoth.Json.Net.Encode.Auto.toString(0, msg.Payload.ContractType))
    "@CreatedBy", Sql.string msg.CurrentUser.Principal
    "@CreatedAt", Sql.timestamp DateTime.UtcNow
    "@LastUpdatedBy", Sql.string msg.CurrentUser.Principal
    "@LastUpdatedAt", Sql.timestamp DateTime.UtcNow
]

let createContract conn (msg: Message<ValidatedContract>) =
    Sql.connect conn
    |> Sql.query 
        """
            INSERT INTO Contracts 
                (ContractId, BuildingId, ContractFileId, ContractOrganizationId, ContractType, CreatedBy, CreatedAt, LastUpdatedBy, LastUpdatedAt)
            VALUES
                (@ContractId, @BuildingId, @ContractFileId, @ContractOrganizationId, @ContractType, @CreatedBy, @CreatedAt, @LastUpdatedBy, @LastUpdatedAt)
        """
    |> Sql.parameters (msg |> toParams)
    |> Sql.writeAsync
    |> Async.Ignore

let updateContract conn (msg: Message<ValidatedContract>) =
    Sql.connect conn
    |> Sql.writeBatchAsync [
        """
            INSERT INTO Contracts_History
                (ContractId, BuildingId, ContractFileId, ContractOrganizationId, ContractType, LastUpdatedBy, LastUpdatedAt)
            VALUES 
                (SELECT 
                    ContractId, BuildingId, ContractFileId, ContractOrganizationId, ContractType, LastUpdatedBy, LastUpdatedAt
                    FROM Contracts
                    WHERE ContractId = @ContractId AND BuildingId = @BuildingId)
        """, [ "@ContractId", Sql.uuid msg.Payload.ContractId; "@BuildingId", Sql.uuid msg.Payload.BuildingId ]

        """
            UPDATE Contracts
                SET 
                    ContractFileId = @ContractFileId
                    ContractOrganizationId = @ContractOrganizationId
                    ContractType = @ContractType
                WHERE
                    ContractId = @ContractId AND BuildingId = @BuildingId;
        """, msg |> toParams
    ]
    |> Async.map (List.skip 1 >> List.head)

let deleteContract conn (msg: Message<BuildingId * Guid>) =
    Sql.connect conn
    |> Sql.writeBatchAsync [
        """
            INSERT INTO Contracts_History
                (ContractId, BuildingId, ContractFileId, ContractOrganizationId, ContractType, LastUpdatedBy, LastUpdatedAt)
            VALUES 
                (SELECT 
                    ContractId, BuildingId, ContractFileId, ContractOrganizationId, ContractType, LastUpdatedBy, LastUpdatedAt
                    FROM Contracts
                    WHERE ContractId = @ContractId AND BuildingId = @BuildingId)
        """, [ "@ContractId", Sql.uuid (snd msg.Payload); "@BuildingId", Sql.uuid (fst msg.Payload) ]

        """
            UPDATE Contracts
                SET 
                    IsActive = FALSE, LastUpdatedBy = @LastUpdatedBy, LastUpdatedAt = @LastUpdatedAt
                WHERE
                    ContractId = @ContractId AND BuildingId = @BuildingId;
        """, [ 
            "@ContractId", Sql.uuid (snd msg.Payload)
            "@BuildingId", Sql.uuid (fst msg.Payload)
            "@LastUpdatedBy", Sql.string msg.CurrentUser.Principal
            "@LastUpdatedAt", Sql.timestamp DateTime.UtcNow
        ]    
    ]
    |> Async.map (List.skip 1 >> List.head)

let saveContractTypeAnswer conn (answer: ContractTypeAnswer) =
    Sql.connect conn
    |> Sql.query 
        """
            INSERT INTO ContractTypeAnswers (BuildingId, Question, IsTrue) VALUES (@BuildingId, @Question, @IsTrue)
            ON CONFLICT ON CONSTRAINT contracttypeanswers_pkey DO UPDATE SET IsTrue = @IsTrue
        """
    |> Sql.parameters [
        "@BuildingId", Sql.uuid answer.BuildingId
        "@Question", Sql.string (string answer.Question)
        "@IsTrue", Sql.bool answer.IsTrue
    ]
    |> Sql.writeAsync

let makeStorage conn = {
    new IContractStorage with
        member _.CreateContract msg = createContract conn msg
        member _.UpdateContract msg = updateContract conn msg
        member _.DeleteContract msg = deleteContract conn msg
        member _.SaveContractTypeAnswer answer = saveContractTypeAnswer conn answer
}