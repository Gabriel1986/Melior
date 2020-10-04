namespace Server

open System
open Microsoft.AspNetCore.Http
open Thoth.Json.Net
open Shared.Read
open Serilog

module Library =
    [<RequireQualifiedAccess>]
    module Option =
        let either fIsSome fIsNone opt =
            match opt with
            | Some x -> fIsSome x
            | None   -> fIsNone

        let fromResult =
            function
            | Ok r -> Some r
            | Error _ -> None

    [<RequireQualifiedAccess>]
    module Async =
        let map f op = async {
            let! x    = op
            let value = f x
            return value
        }

        let lift x = async {
            return x
        }

        let bind fx x = async {
            let! y = x
            return! fx y
        }
            
    type Message<'T> = {
        CreatedAt: DateTimeOffset
        Context: HttpContext
        Payload: 'T
    }

    module Message =
        let map newPayload message = {
            CreatedAt = message.CreatedAt
            Context = message.Context
            Payload = newPayload
        }

    type ValidationError = 
        {
            Path: string
            Message: string
        }
        static member Of path message = {
            Path = path
            Message = message
        }

    [<RequireQualifiedAccess>]
    module BankAccount =
        let private bankAccountEncoder = Encode.Auto.generateEncoderCached<BankAccount>()
        let private bankAccountDecoder = Decode.Auto.generateDecoderCached<BankAccount>()
        let private bankAccountListEncoder = Encode.Auto.generateEncoderCached<BankAccount list>()
        let private bankAccountListDecoder = Decode.Auto.generateDecoderCached<BankAccount list>()
    
        let toJson (address: BankAccount) =
            Encode.toString 0 (bankAccountEncoder address)
    
        let fromJson (str: string) =
            Decode.fromString bankAccountDecoder str
    
        let listToJson (addresses: BankAccount list) =
            Encode.toString 0 (bankAccountListEncoder addresses)
    
        let listFromJson (str: string) =
            match Decode.fromString bankAccountListDecoder str with
            | Ok result ->
                result
            | Error e ->
                Log.Logger.Error (e)
                []