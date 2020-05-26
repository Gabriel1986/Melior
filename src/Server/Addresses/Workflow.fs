module Server.Addresses.Workflow

open Thoth.Json.Net
open Shared.Trial.Control
open Shared.Domain
open Shared.ConstrainedTypes
open Server.Addresses.Library

type ValidatedAddress = 
    {
        Street: String255
        ZipCode: String16
        Town: String64
        Country: String64
    }

[<RequireQualifiedAccess>]
module ValidatedAddress =
    let toAddress (validated: ValidatedAddress): Address = { 
        Street = string validated.Street
        ZipCode = string validated.ZipCode
        Town = string validated.Town 
        Country = string validated.Country
    }

    let toJson (validated: ValidatedAddress): string =
        validated |> toAddress |> Address.toJson

    let listToJson (validated: ValidatedAddress list): string =
        validated |> List.map toAddress |> Address.listToJson

let validateAddress (address: Address) = trial {
    from street in String255.Of address.Street
    also zipCode in String16.Of address.ZipCode
    also town in String64.Of address.Town
    also country in String64.Of address.Country
    yield {
        Street = street
        ZipCode = zipCode
        Town = town
        Country = country
    }
}