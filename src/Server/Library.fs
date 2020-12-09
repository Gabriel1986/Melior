namespace Server

open System
open Microsoft.AspNetCore.Http

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
        Context: HttpContext option
        Payload: 'T
    }

    module Message =
        let replacePayload payload msg = {
            CreatedAt = msg.CreatedAt
            Payload = payload
            Context = msg.Context
        }

    let inMsg msg payload = msg |> Message.replacePayload payload

    type ValidationError = 
        {
            Path: string
            Message: string
        }
        static member Of path message = {
            Path = path
            Message = message
        }