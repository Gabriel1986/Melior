module Server.Warnings.Query

open System
open Npgsql.FSharp

open Server.Library
open Server.PostgreSQL
open Shared.Read

let warningsForLots (conn: string) (buildingId: Guid) = async {
    let! actualTotal, expectedTotal =
        Sql.connect conn
        |> Sql.query 
            """
                SELECT COALESCE(SUM(Share), 0) AS ActualTotal, COALESCE(SharesTotal, 0) As ExpectedTotal
                FROM Lots lot
                JOIN Buildings building ON lot.BuildingId = building.BuildingId
                WHERE lot.IsActive = TRUE AND building.BuildingId = @BuildingId
                GROUP BY building.BuildingId
            """
        |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
        |> Sql.readSingle (fun reader -> reader.int "ActualTotal", reader.int "ExpectedTotal")
        |> Async.map (Option.defaultValue (0, 0))

    return [
        if actualTotal <> expectedTotal then 
            {
                Concept = Concept.Lot
                Message =
                    sprintf
                        "De som van de quotiteiten van de kavels (%i) komt niet overeen met het verwachte totaal gedefinieerd op het gebouw (%i)" 
                        actualTotal 
                        expectedTotal
            }
    ]
}

let warningsForContracts (conn: string) (buildingId: Guid) = async {
    let! contractAnswers = Server.Contracts.Query.getContractTypeAnswers conn buildingId
    let allContractTypeQuestions = 
        ContractTypeQuestion.AllValues ()
        |> Set.ofArray
    let answeredQuestions =
        contractAnswers
        |> List.map (fun a -> a.Question)
        |> Set.ofList

    let mandatoryTypes = 
        contractAnswers
        |> List.toArray
        |> Array.collect mandatoryContractTypesFor
        |> Array.append MandatoryContractTypes
        |> Set.ofArray
    let! filledInTypes = 
        Server.Contracts.Query.getFilledInPredefinedContractTypes conn buildingId
        |> Async.map Set.ofList

    return [
        if allContractTypeQuestions - answeredQuestions <> Set.empty then
            {
                Concept = Concept.Contract
                Message = "Gelieve de instellingen van het gebouw te vervolledigen"
            }
        if mandatoryTypes - filledInTypes <> Set.empty then
            {
                Concept = Concept.Contract
                Message = "Niet alle verplichte contracten werden ingevuld"
            }
    ]
        
}

let getWarnings (conn: string) (buildingId: Guid) = async {
    let! warningsForLots = warningsForLots conn buildingId
    let! warningsForContracts = warningsForContracts conn buildingId
    return [
        yield! warningsForLots
        yield! warningsForContracts
    ]
}