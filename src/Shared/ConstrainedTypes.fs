module Shared.ConstrainedTypes

open System
open Shared

let private validateStringOfLength (length: int) (s: string) =
    if s = null
    then Trial.ofError "De waarde is verplicht"
    elif s.Length > length 
    then Trial.ofError "De waarde heeft een lengte groter dan 255"
    else Trial.Pass s

type String16 =
    private | String16 of string
    override me.ToString() = 
        match me with | String16 x -> x


module String16 =
    let Of = validateStringOfLength 16 >> Trial.map String16

type String32 =
    private | String32 of string
    override me.ToString() = 
        match me with | String32 x -> x

module String32 =
    let Of = validateStringOfLength 32 >> Trial.map String32

type String64 =
    private | String64 of string
    override me.ToString() =
        match me with | String64 x -> x

module String64 =
    let Of = validateStringOfLength 64 >> Trial.map String64

type String255 =
    private | String255 of string
    override me.ToString() = 
        match me with | String255 x -> x

module String255 =
    let Of = validateStringOfLength 255 >> Trial.map String255

type PositiveInt = 
    private | PositiveInt of int
    member me.Value = match me with | PositiveInt x -> x

module PositiveInt =
    let Of (x: int) =
        if x >= 0 
        then Trial.Pass (PositiveInt x) 
        else Trial.ofError "De waarde moet groter zijn dan 0..."

/// String of 4 digits + 3 digits + 3 digits (xxxx.xxx.xxx)
type OrganizationNumber = 
    private | OrganizationNumber of string * string * string
    override me.ToString() = 
        match me with | OrganizationNumber (x, y, z) -> sprintf "%s.%s.%s" x y z
    member me.ToStrings() =
        match me with | OrganizationNumber (x, y, z) -> x, y, z

module OrganizationNumber =
    let private IsStringOfLength (length: int) (s: string) =
        not (String.IsNullOrEmpty(s)) && s.Length = length

    let Of (x: string, y: string, z: string) =
        if x |> IsStringOfLength 4 && y |> IsStringOfLength 3 && z |> IsStringOfLength 3 
        then
            Trial.Pass (OrganizationNumber (x, y, z))
        else
            Trial.ofError "Het organisatienummer is verkeerd geformatteerd, verwacht (XXXX.XXX.XXX)"

    let OfString (x: string) =
        if String.IsNullOrEmpty(x) 
        then None
        else 
            let split = x.Split('.')
            if split.Length <> 3 
            then None
            else 
                Of (split.[0], split.[1], split.[2]) 
                |> Trial.toOption