module Server.StorageEngine.OwnerStorage

open System
open Npgsql.FSharp
open Shared.Read
open Shared.Write
open Server.Library
open Server.PostgreSQL
open Server.Blueprint.Data.Storage

let private paramsFor (validated: ValidatedOwner) = [
    "@PersonId"  , Sql.uuid validated.Person.PersonId
    "@BuildingId", Sql.uuid validated.BuildingId
    "@IsResident", Sql.bool validated.IsResident
]

let transformEventToSql (msg: Message<OwnerEvent>) =
    match msg.Payload with
    | OwnerEvent.OwnerEvent event ->
        match event with
        | BuildingSpecificCUDEvent.Created validated ->
            [
                yield! PersonStorage.transformEventToSql (PersonEvent.PersonEvent (CUDEvent.Created validated.Person) |> inMsg msg)

                "INSERT INTO Owners (PersonId, BuildingId, IsResident) VALUES (@PersonId, @BuildingId, @IsResident)"
                , [ paramsFor validated ]
            ]
        | BuildingSpecificCUDEvent.Updated validated ->
            [
                yield! PersonStorage.transformEventToSql (PersonEvent.PersonEvent (CUDEvent.Updated validated.Person) |> inMsg msg)
                
                "UPDATE Owners SET IsResident = @IsResident WHERE PersonId = @PersonId AND BuildingId = @BuildingId"
                , [ paramsFor validated ]
            ]
        | BuildingSpecificCUDEvent.Deleted (buildingId: BuildingId, ownerId: Guid) ->
            [
                "UPDATE Owners SET IsActive = FALSE WHERE PersonId = @PersonId AND BuildingId = @BuildingId"
                , [[
                    "@PersonId", Sql.uuid ownerId 
                    "@BuildingId", Sql.uuid buildingId
                ]]
            ]