module Shared.Library

open System
open System.Collections.Generic
open Trial

let validateOptional (validate: 'T -> Trial<'U, _>) (x: 'T option) =
    match x with
    | Some x -> validate x |> Trial.map Some
    | None -> Trial.Pass None

module List =
    let tryMaxBy (projection: 'T -> 'U) (list: 'T list) =
        match list with
        | [] -> None
        | list -> list |> List.maxBy projection |> Some

    let tryMinBy (projection: 'T -> 'U) (list: 'T list) =
        match list with
        | [] -> None
        | list -> list |> List.minBy projection |> Some

module String =
    let toOption s = if String.IsNullOrEmpty(s) then None else Some s
    let joinWith s (collection: string seq) = String.Join(s, collection)
    let joinOptionsWith s (collection: string option seq) =
        collection |> Seq.choose id |> joinWith s

    let chunk length (xs: string) =
        xs 
        |> Seq.chunkBySize length 
        |> Seq.map String

let parseInt str =
    match str |> String.toOption with
    | Some s ->
        let isParsed, parsed = s |> String.filter Char.IsDigit |> Int32.TryParse
        if isParsed then Some parsed else None
    | None ->
        None

type CreateOrUpdate = Create | Update

module Option =
    let either withSome defaultValue opt =
        match opt with
        | Some x -> withSome x
        | None -> defaultValue

module Async =
    let lift thing = async {
        return thing
    }

type IDictionary<'TKey, 'TValue> with
    member this.TryFind (key: 'TKey): 'TValue option =
        match this.TryGetValue(key) with
        | true, value -> Some value
        | false, _    -> None