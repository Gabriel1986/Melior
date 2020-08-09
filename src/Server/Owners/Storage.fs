module Server.Owners.Storage

open System
open Npgsql.FSharp
open Server.PostgreSQL
open Shared.Read
open Shared.Write

let private paramsFor (validated: ValidatedOwner) =
    [
        "@PersonId"  , Sql.uuid validated.Person.PersonId
        "@BuildingId", Sql.uuid validated.BuildingId
        "@IsResident", Sql.bool validated.IsResident
    ]

[<NoComparison; NoEquality>]
type IOwnerStorage =
    abstract CreateOwner: ValidatedOwner -> Async<unit>
    abstract UpdateOwner: ValidatedOwner -> Async<int>
    abstract DeleteOwner: BuildingId * ownerId: Guid  -> Async<int>

let createOwner (connectionString: string) (validated: ValidatedOwner) =
    Sql.connect connectionString
    |> Sql.writeBatchAsync [
        Server.Persons.Storage.createQuery, 
            Server.Persons.Storage.paramsFor validated.Person

        """
            INSERT INTO Owners (
                PersonId,
                BuildingId,
                IsResident
            ) VALUES (
                @PersonId,
                @BuildingId,
                @IsResident
            )
        """, paramsFor validated
    ]
    |> Async.Ignore

let updateOwner (connectionString: string) (validated: ValidatedOwner) =
    Sql.connect connectionString
    |> Sql.writeBatchAsync [
        Server.Persons.Storage.updateQuery,
            Server.Persons.Storage.paramsFor validated.Person

        """
            UPDATE Owners 
            SET IsResident = @IsResident
            WHERE PersonId = @PersonId
            AND BuildingId = @BuildingId
        """, paramsFor validated
    ]
    |> Async.map (List.tryHead >> Option.defaultValue 0)

let deleteOwner connectionString (buildingId, ownerId) =
    Sql.connect connectionString
    |> Sql.query
        """
            UPDATE Owners
            SET IsActive = FALSE
            WHERE PersonId = @PersonId
            AND BuildingId = @BuildingId
        """
    |> Sql.parameters [ 
        "@PersonId", Sql.uuid ownerId 
        "@BuildingId", Sql.uuid buildingId
    ]
    |> Sql.writeAsync

let makeStorage conn = {
    new IOwnerStorage with
        member _.CreateOwner owner = createOwner conn owner
        member _.UpdateOwner owner = updateOwner conn owner
        member _.DeleteOwner (buildingId, ownerId) = deleteOwner conn (buildingId, ownerId)
}