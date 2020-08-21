﻿module Shared.ConstrainedTypes

open System
open Shared.Trial

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
    member me.Value () = match me with | PositiveInt x -> x

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
            Trial.ofError (path, "Het ondernemingsnummer is verkeerd geformatteerd, verwacht (XXXX.XXX.XXX)")

    let OfString path (x: string) =
        let split = x.Split('.')
        if split.Length <> 3 
        then Of path ("", "", "")
        else Of path (split.[0], split.[1], split.[2])

type VatNumber = 
    private | VatNumber of countryCode: string * vatNumber: string
    member me.CountryCode () = match me with | VatNumber (c, _) -> c
    member me.Value () = match me with | VatNumber (_, v) -> v
    override me.ToString () = 
        match me with 
        | VatNumber (c, v) -> 
            if c = "BE" then
                sprintf "%s %s.%s.%s" c v.[0..3] v.[4..6] v.[7..9]
            else
                sprintf "%s %s" c v

module VatNumber =
    let private parseInt (s: string) = 
        match Int32.TryParse(s) with
        | true, integer -> integer
        | false, _      -> 0

    let Of path (countryCode: string) (vatNumber: string) =
        let isValid =
            if String.IsNullOrWhiteSpace (countryCode) || String.IsNullOrWhiteSpace (vatNumber) then
                false
            elif countryCode = "BE" then
                let digits = vatNumber |> String.filter Char.IsDigit
                let validateBelgianCheckNumber (str: string) =
                    let digits = str.PadLeft(10, '0')
                    let number = parseInt (digits.[0..7])
                    let checkNumber = parseInt (digits.[8..])
                    (number + checkNumber) % 97 = 0
                digits.Length >= 9 && digits.Length <= 10 && validateBelgianCheckNumber digits
            else
                //Let Vies worry about the actual validity... 
                //There are too many ways to validate VAT numbers which differ for every EU member
                //I don't want to update this file whenever a memberstate decides to use different rules
                true

        if isValid then
            Trial.Pass (VatNumber (countryCode, vatNumber |> String.filter (fun character -> Char.IsDigit character)))
        else
            Trial.ofError (path, "Het btw nummer is verkeerd geformatteerd, verwacht bvb. (BE XXX.XXX.XXX) of (BE0123456789)")

    let OfString path (x: string) =
        if x.Length < 2 then
            Of path x ""
        else
            Of path x.[0..1] (x.[2..].Replace(" ", ""))

    let TryParse (x: string) =
        OfString "" x
        |> Trial.toResult