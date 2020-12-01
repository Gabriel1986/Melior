module Server.Contracts.Query

open System
open Npgsql.FSharp
open Server.PostgreSQL
open Shared.Read
open Shared.MediaLibrary

type ContractDbType = {
    ContractId: Guid
    BuildingId: Guid
    ContractFileId: Guid option
    ContractOrganizationId: Guid option
    ContractType: ContractContractType
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
    |> Sql.read (fun reader -> reader.string "ContractType" |> Thoth.Json.Net.Decode.Auto.unsafeFromString<ContractContractType>)
    |> Async.map (List.choose (function | PredefinedContractType predefined -> Some predefined | _ -> None))

let getContracts conn (buildingId: BuildingId) = async {
    let! contracts = 
        Sql.connect conn
        |> Sql.query 
            """
                SELECT ContractId, BuildingId, ContractFileId, ContractOrganizationId, ContractType 
                FROM Contracts 
                WHERE BuildingId = @BuildingId AND IsActive = TRUE
            """
        |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
        |> Sql.read (fun reader -> {
            ContractId = reader.uuid "ContractId"
            BuildingId = reader.uuid "BuildingId"
            ContractFileId = reader.uuidOrNone "ContractFileId"
            ContractOrganizationId = reader.uuidOrNone "ContractOrganizationId"
            ContractType = reader.string "ContractType" |> Thoth.Json.Net.Decode.Auto.unsafeFromString<ContractContractType>
        })

    let! mediaFiles = 
        contracts 
        |> List.choose (fun c -> c.ContractFileId) 
        |> (Server.Media.Query.getMediaFilesByIds conn Partitions.Contracts)

    let getMediaFileById mediaFileId = 
        mediaFileId 
        |> Option.bind (fun mediaFileId -> mediaFiles |> List.tryFind (fun m -> m.FileId = mediaFileId))

    let! organizations = 
        contracts 
        |> List.choose (fun c -> c.ContractOrganizationId) |> (Server.Organizations.Query.getOrganizationsByIds conn)

    let getOrganizationById organizationId = 
        organizationId 
        |> Option.bind (fun organizationId -> organizations |> List.tryFind (fun o -> o.OrganizationId = organizationId))

    return contracts |> List.map (fun c -> {
        ContractId = c.ContractId
        BuildingId = c.BuildingId
        ContractFile = getMediaFileById c.ContractFileId
        ContractOrganization = getOrganizationById c.ContractOrganizationId
        ContractType = c.ContractType    
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