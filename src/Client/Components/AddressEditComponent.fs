module Client.Components.AddressEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish.React

open Shared.Read
open Shared.Library
open Client.ClientStyle
open Client.ClientStyle.Helpers

let render (address: Address) (onChange: (Address -> unit) option) (basePath: string) (errors: (string * string) list) =
    let findError x = 
        errors |> List.tryPick (fun (path, error) -> if path = (sprintf "%s.%s" basePath x) then Some error else None)
        
    [
        div [ classes [ Bootstrap.row; "full-width" ] ] [
            div [ Class Bootstrap.colMd8 ] [
                formGroup [
                    Label "Straat + nr"
                    Input [
                        Type "text"
                        MaxLength 255.0 
                        Helpers.valueOrDefault address.Street
                        match onChange with
                        | Some onChange -> OnChange (fun e -> { address with Street = e.Value |> String.toOption } |> onChange)
                        | None -> Disabled true
                    ] 
                    FieldError (findError (nameof address.Street))
                ]
            ]
            div [ Class Bootstrap.colMd4 ] [
                formGroup [ 
                    Label "Bus"
                    Input [ 
                        Type "text"
                        MaxLength 16.0 
                        Helpers.valueOrDefault address.MailboxNumber
                        match onChange with
                        | Some onChange -> OnChange (fun e -> { address with MailboxNumber = e.Value |> String.toOption } |> onChange)
                        | None -> Disabled true
                    ] 
                    FieldError (findError (nameof address.MailboxNumber))
                ]
            ]
        ]
        div [ classes [ Bootstrap.row; "full-width" ] ] [
            div [ Class Bootstrap.colMd3 ] [
                formGroup [ 
                    Label "Postcode"
                    Input [ 
                        Type "text"
                        MaxLength 12.0 
                        Helpers.valueOrDefault address.ZipCode
                        match onChange with
                        | Some onChange -> OnChange (fun e -> { address with ZipCode = e.Value |> String.toOption } |> onChange)
                        | None -> Disabled true                        
                    ] 
                    FieldError (findError (nameof address.ZipCode))
                ]
            ]
            div [ Class Bootstrap.col ] [
                formGroup [ 
                    Label "Woonplaats"
                    Input [ 
                        Type "text"
                        MaxLength 255.0
                        Helpers.valueOrDefault address.Town
                        match onChange with
                        | Some onChange -> OnChange (fun e -> { address with Town = e.Value |> String.toOption } |> onChange)
                        | None -> Disabled true                        
                    ] 
                    FieldError (findError (nameof address.Town))
                ]
            ]
            div [ Class Bootstrap.colMd3 ] [
                formGroup [
                    Label "Land"
                    Input [ 
                        Type "text"
                        MaxLength 255.0 
                        Helpers.valueOrDefault address.Country
                        match onChange with
                        | Some onChange -> OnChange (fun e -> { address with Country = e.Value |> String.toOption } |> onChange)
                        | None -> Disabled true
                    ]
                    FieldError (findError (nameof address.Country))
                ]
            ]
        ]
    ]
    |> fragment []