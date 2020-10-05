module Client.Components.BankAccountEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish.React

open Shared.Read
open Client.ClientStyle
open Client.ClientStyle.Helpers

let render (bankAccount: BankAccount) (onChange: BankAccount -> unit) (onDelete: (BankAccount -> unit) option) (basePath: string) (errors: (string * string) list) =
    let findError x = 
        errors |> List.tryPick (fun (path, error) -> if path = (sprintf "%s.%s" basePath x) then Some error else None)
        
    div [ classes [ Bootstrap.row; "full-width" ] ] [
        div [ Class Bootstrap.colMd ] [
            formGroup [
                Label "Naam rekening"
                Input [ 
                    Type "text"
                    MaxLength 255.0 
                    Helpers.valueOrDefault bankAccount.Description
                    OnChange (fun e -> { bankAccount with Description = e.Value } |> onChange)
                ] 
                FormError (findError (nameof bankAccount.Description))
            ]
        ]
        div [ Class Bootstrap.colMd ] [
            formGroup [ 
                Label "IBAN"
                Input [ 
                    Type "text"
                    MaxLength 34.0
                    Helpers.valueOrDefault bankAccount.IBAN
                    OnChange (fun e -> { bankAccount with IBAN = e.Value } |> onChange)
                ] 
                FormError (findError (nameof bankAccount.IBAN))
            ]
        ]
        div [ Class Bootstrap.colMd ] [
            formGroup [ 
                Label "BIC"
                Input [ 
                    Type "text"
                    MaxLength 12.0
                    Helpers.valueOrDefault bankAccount.BIC
                    OnChange (fun e -> { bankAccount with BIC = e.Value } |> onChange)
                ] 
                FormError (findError (nameof bankAccount.BIC))
            ]
        ]
        match onDelete with
        | Some onDelete ->
            div [ Class Bootstrap.colMd ] [
                div [] [
                    label [ Style [ Visibility "hidden" ]; classes [ Bootstrap.dNone; Bootstrap.dMdBlock ] ] [ str "_" ]
                    div [ Class Bootstrap.formGroup ] [
                        button [ 
                            classes [ Bootstrap.btn; Bootstrap.btnDanger ]
                            OnClick (fun _ -> onDelete bankAccount)
                        ] [
                            str "Verwijderen"
                        ]
                    ]
                ]
            ]
        | None ->
            null
    ]