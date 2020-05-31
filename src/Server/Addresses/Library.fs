module Server.Addresses.Library

open Thoth.Json.Net
open Shared.Read

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
        Decode.fromString addressListDecoder str
