module Server.Addresses.Workflow

open Shared.Read
open Shared.Write
open Server.Addresses.Library


[<RequireQualifiedAccess>]
module ValidatedAddress =
    let toAddress (validated: ValidatedAddress): Address = { 
        Street = validated.Street |> Option.map string
        ZipCode = validated.ZipCode |> Option.map string
        Town = validated.Town  |> Option.map string
        Country = validated.Country |> Option.map string
    }

    let toJson (validated: ValidatedAddress): string =
        validated |> toAddress |> Address.toJson

    let listToJson (validated: ValidatedAddress list): string =
        validated |> List.map toAddress |> Address.listToJson