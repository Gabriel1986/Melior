module Server.Warnings.Query

open System
open Npgsql.FSharp

open Server.Library
open Server.PostgreSQL
open Shared.Read

let private warningsForLots (conn: string) (buildingId: Guid) = async {
    let! lotShareCount =
        Sql.connect conn
        |> Sql.query "SELECT COALESCE(SUM(Share), 0) AS Sum FROM Lots WHERE BuildingId = @BuildingId"
        |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
        |> Sql.readSingle (fun reader -> reader.int "Sum")
        |> Async.map (Option.defaultValue 0)

    let! expectedTotal =
        Sql.connect conn
        |> Sql.query "SELECT COALESCE(SharesTotal, 0) AS SharesTotal FROM Buildings WHERE BuildingId = @BuildingId"
        |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
        |> Sql.readSingle (fun reader -> reader.int "SharesTotal")
        |> Async.map (Option.defaultValue 0)

    return [
        if lotShareCount <> expectedTotal then 
            {
                Concept = Concept.Lot
                Type = WarningType
                Message =
                    sprintf
                        "De som van de quotiteiten van de kavels (%i) komt niet overeen met het verwachte totaal gedefinieerd op het gebouw (%i)" 
                        lotShareCount 
                        expectedTotal
            }
    ]
}

let getAllWarnings (conn: string) (buildingId: Guid) = async {
    let! warningsForLots = warningsForLots conn buildingId
    return [
        yield! warningsForLots
    ]
}

let getWarningsForConcept (conn: string) (buildingId: Guid, concept: Concept) = async {
    match concept with
    | Concept.Lot -> return! warningsForLots conn buildingId
}