namespace Server

open System
open Microsoft.AspNetCore.Http
open Thoth.Json.Net
open Shared.Read
open Serilog
open Shared.Write

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
        let private bankAccountDecoder: Decoder<BankAccount> =
            Decode.object(fun get -> {
                Description = get.Required.Field "Description" Decode.string
                IBAN = get.Required.Field "IBAN" Decode.string
                BIC = get.Required.Field "BIC" Decode.string
                Validated = get.Optional.Field "Validated" Decode.bool
            })
        let private bankAccountListEncoder = Encode.Auto.generateEncoderCached<BankAccount list>()
    
        let toJson (bankAccount: BankAccount) =
            Encode.toString 0 (bankAccountEncoder bankAccount)
    
        let fromJson (str: string) =
            match Decode.fromString bankAccountDecoder str with
            | Ok result -> 
                result
            | Error e -> 
                Log.Logger.Error (e)
                failwithf "Decoding bank account has failed."
    
        let listToJson (bankAccounts: BankAccount list) =
            Encode.toString 0 (bankAccountListEncoder bankAccounts)
    
        let listFromJson (str: string) =
            match Decode.fromString (Decode.list bankAccountDecoder) str with
            | Ok result ->
                result
            | Error e ->
                Log.Logger.Error (e)
                []

    [<RequireQualifiedAccess>]
    module ValidatedBankAccount =
        let toBankAccount (validated: ValidatedBankAccount): BankAccount = { 
            Description = match validated.Description with | Some d -> string d | None -> ""
            IBAN = match validated.IBAN with | Some iban -> string iban | None -> ""
            BIC = match validated.BIC with | Some bic -> string bic | None -> ""
            Validated = validated.Validated
        }

        let toJson (validated: ValidatedBankAccount): string =
            validated |> toBankAccount |> BankAccount.toJson

        let listToJson (validated: ValidatedBankAccount list): string =
            validated |> List.map toBankAccount |> BankAccount.listToJson