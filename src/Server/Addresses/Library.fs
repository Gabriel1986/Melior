module Server.Addresses.Library

open Thoth.Json.Net
open Shared.Read
open Serilog
open Shared.Write

[<RequireQualifiedAccess>]
module Address = 
    let private addressEncoder = Encode.Auto.generateEncoderCached<Address>()
    let private addressDecoder = Decode.Auto.generateDecoderCached<Address>()
    let private addressListEncoder = Encode.Auto.generateEncoderCached<Address list>()
    let private addressListDecoder = Decode.Auto.generateDecoderCached<Address list>()
    
    let toJson (address: Address) =
        Encode.toString 0 (addressEncoder address)

    let fromJson (str: string) =
        Decode.fromString addressDecoder str

    let listToJson (addresses: Address list) =
        Encode.toString 0 (addressListEncoder addresses)

    let listFromJson (str: string) =
        match Decode.fromString addressListDecoder str with
        | Ok result ->
            result
        | Error e ->
            Log.Logger.Error (e)
            []

[<RequireQualifiedAccess>]
module ValidatedAddress =
    let toAddress (validated: ValidatedAddress): Address = { 
        Street = validated.Street |> Option.map string
        MailboxNumber = validated.MailboxNumber |> Option.map string
        ZipCode = validated.ZipCode |> Option.map string
        Town = validated.Town  |> Option.map string
        Country = validated.Country |> Option.map string
    }

    let toJson (validated: ValidatedAddress): string =
        validated |> toAddress |> Address.toJson

    let listToJson (validated: ValidatedAddress list): string =
        validated |> List.map toAddress |> Address.listToJson

[<RequireQualifiedAccess>]
module OtherAddress =
    let private addressEncoder = Encode.Auto.generateEncoderCached<OtherAddress>()
    let private addressDecoder = Decode.Auto.generateDecoderCached<OtherAddress>()
    let private addressListEncoder = Encode.Auto.generateEncoderCached<OtherAddress list>()
    let private addressListDecoder = Decode.Auto.generateDecoderCached<OtherAddress list>()

    let toJson (address: OtherAddress) =
        Encode.toString 0 (addressEncoder address)

    let fromJson (str: string) =
        Decode.fromString addressDecoder str

    let listToJson (addresses: OtherAddress list) =
        Encode.toString 0 (addressListEncoder addresses)

    let listFromJson (str: string) =
        Decode.fromString addressListDecoder str

[<RequireQualifiedAccess>]
module ValidatedOtherAddress =
    let toOtherAddress (validated: ValidatedOtherAddress): OtherAddress = {
        Description = validated.Description
        Address = validated.Address |> ValidatedAddress.toAddress
    }

    let toJson (validated: ValidatedOtherAddress): string =
        validated |> toOtherAddress |> OtherAddress.toJson

    let listToJson (validated: ValidatedOtherAddress list): string =
        validated |> List.map toOtherAddress |> OtherAddress.listToJson