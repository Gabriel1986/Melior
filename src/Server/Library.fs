namespace Server

open System
open Microsoft.AspNetCore.Http

module Library =
    type Message<'T> = 
        {
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
    module Option =
        let either fIsSome fIsNone opt =
            match opt with
            | Some x -> fIsSome x
            | None   -> fIsNone

        let fromResult =
            function
            | Ok r -> Some r
            | Error _ -> None

    module Async =
        let map f op = async {
            let! x    = op
            let value = f x
            return value
        }

        let lift x = async {
            return x
        }