module Server.Persons.Library

open Shared.Read
open Thoth.Json.Net

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


