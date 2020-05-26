module Client.Components.AddressEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish.React

open Shared.Domain
open Client.ClientStyle
open Client.ClientStyle.Helpers

let render (addressDescription: string) (address: Address) (onChange: Address -> unit) =
    div [ Class Bootstrap.formGroup ] [
        div [] [
            if not (String.IsNullOrWhiteSpace addressDescription) then
                yield label [] [ str addressDescription ]
        ]

        div [ Class Bootstrap.row ] [
            div [ Class Bootstrap.col ] [
                formGroup [ 
                    Label "Straat + nr"
                    Input [ 
                        Type "text"
                        MaxLength 255.0 
                        Helpers.valueOrDefault address.Street
                        OnChange (fun e -> { address with Street = e.Value } |> onChange)
                    ] 
                ]
            ]
            div [ Class Bootstrap.col ] [
                formGroup [ 
                    Label "Postcode"
                    Input [ 
                        Type "text"
                        MaxLength 12.0 
                        Helpers.valueOrDefault address.ZipCode
                        OnChange (fun e -> { address with ZipCode = e.Value } |> onChange)
                    ] 
                ]
            ]
            div [ Class Bootstrap.col ] [
                formGroup [ 
                    Label "Woonplaats"
                    Input [ 
                        Type "text"
                        MaxLength 255.0
                        Helpers.valueOrDefault address.Town
                        OnChange (fun e -> { address with Town = e.Value } |> onChange)
                    ] 
                ]
            ]
            div [ Class Bootstrap.col ] [
                formGroup [
                    Label "Land"
                    Input [ 
                        Type "text"
                        MaxLength 255.0 
                        Helpers.valueOrDefault address.Country
                        OnChange (fun e -> { address with Country = e.Value } |> onChange)
                    ]
                ]
            ]
        ]
    ]