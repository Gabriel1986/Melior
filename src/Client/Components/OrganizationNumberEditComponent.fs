module Client.Components.OrganizationNumberEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish.React

open Client.ClientStyle
open Client.ClientStyle.Helpers
open Fable.Core

let render (orgNrDescription: string) (orgNr: string * string * string) (onChange: string * string * string -> unit) =
    let forceValue x = if String.IsNullOrWhiteSpace x then "" else x
    let first, second, third = orgNr
    let combined =
        if (first |> forceValue).Length = 4 && (second |> forceValue).Length = 3 && (third |> forceValue).Length = 3 then
            sprintf "%s.%s.%s" first second third
        else
            first
    
    let format (e: Browser.Types.KeyboardEvent) =
        let digitString = e.Value |> forceValue |> String.filter Char.IsDigit
        if digitString.Length = 10 then
            digitString.[0..3], digitString.[4..6], digitString.[7..9]
        else
            e.Value, "", ""
        
    formGroup [
        Label orgNrDescription
        Input [ 
            Type "text"
            Pattern "[0-9]{4}\.[0-9]{3}\.[0-9]{3}"
            MaxLength 12.0
            Placeholder "xxxx.xxx.xxx"
            Helpers.valueOrDefault combined
            OnKeyUp (fun e -> format e |> onChange)
        ] 
    ]