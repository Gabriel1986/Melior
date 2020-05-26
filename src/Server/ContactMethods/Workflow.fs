module Server.ContactMethods.Workflow

open Shared.ConstrainedTypes
open Shared.Domain
open Shared.Trial.Control
open Server.ContactMethods.Library

type ValidatedContactMethod = {
    ContactMethodType: ContactMethodType
    Value: String255
    Description: string
}

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


let validateContactMethod (contactMethod: ContactMethod) = trial {
    from value in String255.Of contactMethod.Value
    yield {
        ContactMethodType = contactMethod.ContactMethodType
        Value = value
        Description = contactMethod.Description
    }
}