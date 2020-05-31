module Server.ContactMethods.Workflow

open Shared.Read
open Shared.Write
open Server.ContactMethods.Library

[<RequireQualifiedAccess>]
module ValidatedContactMethod =
    let toContactMethod (validated: ValidatedContactMethod): ContactMethod = { 
        ContactMethodType = validated.ContactMethodType
        Value = string validated.Value
        Description = validated.Description
    }

    let toJson (validated: ValidatedContactMethod): string =
        validated |> toContactMethod |> ContactMethod.toJson

    let listToJson (validated: ValidatedContactMethod list): string =
        validated |> List.map toContactMethod |> ContactMethod.listToJson