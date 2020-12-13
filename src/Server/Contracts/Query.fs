module Server.Contracts.Query

open System
open Npgsql.FSharp
open Server.PostgreSQL
open Shared.Read
open Shared.MediaLibrary
open Server.Blueprint.Data.Contracts

type DbContract = {
    ContractId: Guid
    BuildingId: Guid
    ContractOrganizationId: Guid option
    ContractType: DbContractType
}

let getFilledInPredefinedContractTypes (conn: string) (buildingId: BuildingId) =
    Sql.connect conn
    |> Sql.query
        """
            SELECT ContractType
            FROM Contracts
            WHERE BuildingId = @BuildingId AND IsActive = TRUE
        """
    |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
    |> Sql.read (fun reader -> reader.string "ContractType" |> Thoth.Json.Net.Decode.Auto.unsafeFromString<DbContractType>)
    |> Async.map (List.choose (function | PredefinedContractType predefined -> Some predefined.Type | _ -> None))

let getContracts conn (buildingId: BuildingId) = async {
    let! contracts = 
        Sql.connect conn
        |> Sql.query 
            """
                SELECT ContractId, BuildingId, ContractOrganizationId, ContractType 
                FROM Contracts 
                WHERE BuildingId = @BuildingId AND IsActive = TRUE
            """
        |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
        |> Sql.read (fun reader -> {
            ContractId = reader.uuid "ContractId"
            BuildingId = reader.uuid "BuildingId"
            ContractOrganizationId = reader.uuidOrNone "ContractOrganizationId"
            ContractType = reader.string "ContractType" |> Thoth.Json.Net.Decode.Auto.unsafeFromString<DbContractType>
        })

    let! mediaFiles = 
        contracts 
        |> List.map (fun c -> c.ContractId)
        |> (Server.Media.Query.getMediaFilesForEntities conn Partitions.Contracts)

    let getMediaFilesByEntityId entityId = 
        mediaFiles |> List.filter (fun m -> m.EntityId = entityId)

    let organizationIds =
        contracts
        |> List.collect (fun contract ->
            List.choose id [
                yield contract.ContractOrganizationId
                match contract.ContractType with
                | InsuranceContractType insuranceContract -> yield insuranceContract.BrokerId
                | PredefinedContractType predefinedContract -> yield predefinedContract.BrokerId
                | _ -> ()
            ])

    let! organizations = Server.Organizations.Query.getOrganizationsByIds conn organizationIds

    let getOrganizationById organizationId = 
        organizationId 
        |> Option.bind (fun organizationId -> organizations |> List.tryFind (fun o -> o.OrganizationId = organizationId))

    return contracts |> List.map (fun c -> {
        ContractId = c.ContractId
        BuildingId = c.BuildingId
        ContractFiles = getMediaFilesByEntityId c.ContractId
        ContractOrganization = getOrganizationById c.ContractOrganizationId
        ContractType = 
            match c.ContractType with
            | OtherContractType name -> ContractType.Other name
            | PredefinedContractType pre -> 
                ContractType.Predefined {
                    Type = pre.Type
                    Broker = getOrganizationById pre.BrokerId
                }
            | InsuranceContractType contractType ->
                ContractType.Insurance {
                    Name = contractType.Name
                    Broker = getOrganizationById contractType.BrokerId
                }
    })
}

let getContractTypeAnswers conn (buildingId: BuildingId) =
    Sql.connect conn
    |> Sql.query "SELECT BuildingId, Question, IsTrue FROM ContractTypeAnswers WHERE BuildingId = @BuildingId"
    |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
    |> Sql.read (fun reader -> 
        match reader.string "Question" |> ContractTypeQuestion.OfString with
        | Some question ->
            Some { 
                BuildingId = reader.uuid "BuildingId"
                Question = question
                IsTrue = reader.bool "IsTrue"
            }
        | None -> None)
    |> Async.map (List.choose id)