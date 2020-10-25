module Server.SeedData.FinancialCategories

open FSharp.Data
open Server.Blueprint.Data.SeedData

type FinancialCategories = CsvProvider<"Code;Description", HasHeaders=true, Separators=";">

let readPredefined () = async {
    let! financialCategories = FinancialCategories.AsyncLoad("SeedData/FinancialCategories.csv")
    return
        financialCategories.Rows
        |> Seq.map (fun row -> { Code = row.Code; Description = row.Description })
}