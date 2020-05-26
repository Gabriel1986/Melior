namespace Shared

open System
    
type Trial<'value, 'error> =
    | Pass of 'value
    | Fail of 'error list

[<RequireQualifiedAccess>]
module Trial =
    let inline dispose (disposable: 'Object when 'Object :> IDisposable) : unit =
        if not (isNull disposable) then disposable.Dispose()

    let inline ofError fail = Fail [ fail ]

    let isPass = function
        | Fail _ -> false
        | Pass _ -> true

    let isFail = function
        | Pass _ -> false
        | Fail _ -> true

    let map mapper = function
        | Fail fails -> Fail fails
        | Pass value -> Pass (mapper value)

    let mapFail mapper = function
        | Pass value -> Pass value
        | Fail fails -> Fail (mapper fails)

    let mapErrors mapper trial =
        trial |> mapFail (List.map mapper)

    let inline map2 merge trial1 trial2 =
        //TODO profile this operation under load (especially for many errors)
        match (trial1, trial2) with
        | (Pass value1, Pass value2) -> Pass (merge value1 value2)
        | (Pass _     , Fail fails )
        | (Fail fails , Pass _     ) -> Fail fails
        | (Fail fails1, Fail fails2) -> Fail (fails1 @ fails2)

    let map3 merge trial1 trial2 trial3 =
        map2 id (map2 id (trial1 |> map merge) trial2) trial3

    let apply trial application =
        map2 (fun apply value -> apply value) application trial

    let either withPass withFail trial =
        match trial with
        | Fail fails -> withFail fails
        | Pass value -> withPass value

    let mapEither withPass withFail trial =
        trial |> either (withPass >> Pass) (withFail >> Fail)

    let traverse traversal items =
        if isNull items || Seq.isEmpty items then
            Pass Seq.empty
        else
            let passed = ResizeArray<_>()
            let failed = ResizeArray<_>()
            use items' = items.GetEnumerator()
            while items'.MoveNext() do
                match traversal items'.Current with
                | Fail error -> failed.AddRange(error)
                | Pass value -> passed.Add(value)
            if 0 < failed.Count
                then Fail (Seq.toList   failed)
                else Pass (Seq.readonly passed)

    let sequence items =
        items |> traverse id

    let iter action trial =
        match trial with
        | Fail _     -> ()
        | Pass value -> action value

    let iterFail action = function
        | Pass _     -> ()
        | Fail fails -> action fails

    let iterErrors action trial =
        trial |> iterFail (List.iter action)

    let ofResult = function
        | Ok    value -> Pass  value
        | Error error -> Fail [error]

    let toResult = function
        | Pass value -> Ok value
        | Fail fails -> Error fails

    let ofChoice = function
        | Choice1Of2 value -> Pass value
        | Choice2Of2 error -> Fail error

    let toChoice = function
        | Pass value -> Choice1Of2 value
        | Fail fails -> Choice2Of2 fails

    let toOption = function
        | Pass value -> Some value
        | Fail _     -> None

    module Operators =

        let inline ( <!> ) mapper result = map mapper result

        let inline ( <*> ) application result = apply result application

    module Control =
        [<Sealed>]
        type TrialBuilder() =
            member _.Delay(generator: unit -> Trial<_, _>) = generator

            member _.Run(generator: unit -> Trial<_, _>) = generator ()

            member _.Yield(value) = Pass value

            member _.YieldFrom(trial: Trial<_, _>) = trial

            member _.For(trial, body) =
                match trial with
                | Fail fails -> Fail fails
                | Pass value -> body value

            [<CustomOperation("from", IsLikeZip = true)>]
            member inline __.Lift(trial1, trial2, merge: unit -> _ -> _) =
                map2 merge trial1 trial2

            [<CustomOperation("also", IsLikeZip = true)>]
            member inline __.Merge(trial1, trial2, merge) =
                map2 merge trial1 trial2

            member inline __.TryWith(body, handler) : Trial<_, _> =
                try body () with x -> handler x

            member inline __.TryFinally(body, handler) : Trial<_, _> =
                try body () finally handler ()

            member _.Using(resource, body) : Trial<_, _> =
                try body resource finally dispose resource

            member me.While(guard, body) =
                if not (guard ()) then
                    body ()
                else
                    match body () with
                    | Fail fails -> Fail fails
                    | Pass _     -> me.While(guard, body)

        let trial = TrialBuilder()