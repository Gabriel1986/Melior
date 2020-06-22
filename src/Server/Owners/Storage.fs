module Server.Owners.Storage

open Npgsql.FSharp
open Server.PostgreSQL
open Shared.Write

let private paramsFor (validated: ValidatedOwner) =
    [
        "@PersonId"  , Sql.uuid validated.Person.PersonId
        "@BuildingId", Sql.uuid validated.BuildingId
        "@IsResident", Sql.bool validated.IsResident
    ]

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

let updateOwner (connectionString: string) (validated: ValidatedOwner) =
    Sql.connect connectionString
    |> Sql.writeBatchAsync [
        Server.Persons.Storage.updateQuery,
            Server.Persons.Storage.paramsFor validated.Person

        """
            UPDATE Owners 
            SET IsResident = @IsResident
            WHERE PersonId = @PersonId
        """, paramsFor validated
    ]

let deleteOwner connectionString lotId =
    Sql.connect connectionString
    |> Sql.query
        """
            UPDATE Owners
            SET IsActive = FALSE
            WHERE PersonId = @PersonId
        """
    |> Sql.parameters [ "@PersonId", Sql.uuid lotId ]
    |> Sql.writeAsync