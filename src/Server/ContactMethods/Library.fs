module Server.ContactMethods.Library

open Thoth.Json.Net
open Shared.Domain

[<RequireQualifiedAccess>]
module ContactMethod =
    let private contactMethodEncoder = Encode.Auto.generateEncoderCached<ContactMethod>()
    let private contactMethodDecoder = Decode.Auto.generateDecoderCached<ContactMethod>()
    let private contactMethodListEncoder = Encode.Auto.generateEncoderCached<ContactMethod list>()
    let private contactMethodListDecoder = Decode.Auto.generateDecoderCached<ContactMethod list>()
    
    let toJson (contactMethod: ContactMethod) =
        Encode.toString 0 (contactMethodEncoder contactMethod)
    
    let fromJson (str: string) =
        Decode.fromString contactMethodDecoder str
    
    let listToJson (contactMethods: ContactMethod list) =
        Encode.toString 0 (contactMethodListEncoder contactMethods)
    
    let listFromJson (str: string) =
        Decode.fromString contactMethodListDecoder str