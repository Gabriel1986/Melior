module Shared.ConstrainedTypes

open System
open Shared

let private validateStringOfLength path (length: int) (s: string) =
    if String.IsNullOrEmpty (s)
    then Trial.ofError (path, "De waarde is verplicht")
    elif s.Length > length 
    then Trial.ofError (path, sprintf "De waarde heeft een lengte groter dan %i" length)
    else Trial.Pass s

type String16 =
    private | String16 of string
    override me.ToString() = 
        match me with | String16 x -> x


module String16 =
    let Of path = validateStringOfLength path 16 >> Trial.map String16

type String32 =
    private | String32 of string
    override me.ToString() = 
        match me with | String32 x -> x

module String32 =
    let Of path = validateStringOfLength path 32 >> Trial.map String32

type String64 =
    private | String64 of string
    override me.ToString() =
        match me with | String64 x -> x

module String64 =
    let Of path = validateStringOfLength path 64 >> Trial.map String64

type String255 =
    private | String255 of string
    override me.ToString() = 
        match me with | String255 x -> x

module String255 =
    let Of path = validateStringOfLength path 255 >> Trial.map String255

type PositiveInt = 
    private | PositiveInt of int
    member me.Value = match me with | PositiveInt x -> x

let validatePositiveInt path x =
    if x < 0 then Trial.ofError (path, "De waarde moet groter zijn dan 0...") else Trial.Pass (PositiveInt x)

module PositiveInt =
    let Of path = validatePositiveInt path

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

    let Of path (x: string, y: string, z: string) =
        if x |> IsStringOfLength 4 && y |> IsStringOfLength 3 && z |> IsStringOfLength 3 
        then
            Trial.Pass (OrganizationNumber (x, y, z))
        else
            Trial.ofError (path, "Het organisatienummer is verkeerd geformatteerd, verwacht (XXXX.XXX.XXX)")

    let OfString path (x: string) =
        let split = x.Split('.')
        if split.Length <> 3 
        then Of path ("", "", "")
        else Of path (split.[0], split.[1], split.[2])