module Server.SeedData.FinancialCategories

open System
open FSharp.Data
open Shared.Read

type FinancialCategories = CsvProvider<"Code;Description", HasHeaders=true, Separators=";">

let readPredefined (buildingId: Guid) = async {
    let! financialCategories = FinancialCategories.AsyncLoad("SeedData/FinancialCategories.csv")
    return
        financialCategories.Rows
        |> Seq.map (fun row -> { 
            FinancialCategory.FinancialCategoryId = System.Guid.NewGuid()
            FinancialCategory.Code = row.Code
            FinancialCategory.Description = row.Description
            FinancialCategory.BuildingId = buildingId
            FinancialCategory.LotOwnerId = None
        })
        |> List.ofSeq
}