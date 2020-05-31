module Client.Components.AddressEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish.React

open Shared.Read
open Shared.Library
open Client.ClientStyle
open Client.ClientStyle.Helpers

let render (addressDescription: string) (address: Address) (onChange: Address -> unit) (basePath: string) (errors: (string * string) list) =
    let findError x = 
        errors |> List.tryPick (fun (path, error) -> if path = (sprintf "%s.%s" basePath x) then Some error else None)
        
    div [ Class Bootstrap.formGroup ] [
        div [] [
            if not (String.IsNullOrWhiteSpace addressDescription) then
                yield label [] [ str addressDescription ]
        ]

        div [ Class Bootstrap.row ] [
            div [ Class Bootstrap.colMd5 ] [
                formGroup [ 
                    Label "Straat + nr"
                    Input [ 
                        Type "text"
                        MaxLength 255.0 
                        Helpers.valueOrDefault address.Street
                        OnChange (fun e -> { address with Street = e.Value |> String.toOption } |> onChange)
                    ] 
                    FormError (findError (nameof address.Street))
                ]
            ]
            div [ Class Bootstrap.col ] [
                formGroup [ 
                    Label "Postcode"
                    Input [ 
                        Type "text"
                        MaxLength 12.0 
                        Helpers.valueOrDefault address.ZipCode
                        OnChange (fun e -> { address with ZipCode = e.Value |> String.toOption } |> onChange)
                    ] 
                    FormError (findError (nameof address.ZipCode))
                ]
            ]
            div [ Class Bootstrap.colMd4 ] [
                formGroup [ 
                    Label "Woonplaats"
                    Input [ 
                        Type "text"
                        MaxLength 255.0
                        Helpers.valueOrDefault address.Town
                        OnChange (fun e -> { address with Town = e.Value |> String.toOption } |> onChange)
                    ] 
                    FormError (findError (nameof address.Town))
                ]
            ]
            div [ Class Bootstrap.col ] [
                formGroup [
                    Label "Land"
                    Input [ 
                        Type "text"
                        MaxLength 255.0 
                        Helpers.valueOrDefault address.Country
                        OnChange (fun e -> { address with Country = e.Value |> String.toOption } |> onChange)
                    ]
                    FormError (findError (nameof address.Country))
                ]
            ]
        ]
    ]