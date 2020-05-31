module Shared.Library

open System

let validateOptional (validate: 'T -> Trial<'U, _>) (x: 'T option) =
    match x with
    | Some x -> validate x |> Trial.map Some
    | None -> Trial.Pass None

module String =
    let toOption s = if String.IsNullOrEmpty(s) then None else Some s

type CreateOrUpdate = Create | Update