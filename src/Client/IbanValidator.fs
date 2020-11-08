module Client.IbanValidator

open System
open Fable.Core
open Fable.SimpleJson
open Fable.SimpleHttp

type IbanValidationResult = {
    valid: bool
    messages: string array option
    iban: string
    bankData: {| bic: string |}
}

type IIbanValidator =
    abstract isValid: iban: string -> bool
    abstract toBBAN: iban: string -> separator: string -> string
    abstract fromBBAN: countryCode: string -> bban: string -> string
    abstract isValidBBAN: countryCode: string -> bban: string -> bool
    abstract printFormat: iban: string -> separator: string -> string
    abstract electronicFormat: iban: string -> string

[<Import("*", from="iban")>]
let ibanValidator: IIbanValidator = jsNative

let private getBicNumber (iban: string): Async<IbanValidationResult> = async {
    let! response =
        Http.request (sprintf "https://openiban.com/validate/%s?getBIC=true&validateBankCode=true" iban)
        |> Http.method GET
        |> Http.header (Headers.contentType "application/json")
        |> Http.send

    match response.content with
    | ResponseContent.Text json ->
        match Json.tryParseAs<IbanValidationResult>(json) with
        | Ok validationResult ->
            return validationResult
        | Error e ->
            return { 
                valid = false
                messages = Some [| e |]
                iban = iban
                bankData = {| bic = "" |} 
            }
    | other ->
        Browser.Dom.console.error("Actual response content: ", other)
        return { 
            valid = false
            messages = Some [| "Er is iets misgelopen bij het uitlezen van de validatie" |]
            iban = iban
            bankData = {| bic = "" |} 
        }
}

let validateIban (iban: string): Async<IbanValidationResult> = async {
    if ibanValidator.isValid(iban) then
        return! getBicNumber (iban |> String.filter Char.IsLetterOrDigit)
    else
        return { 
            valid = false
            messages = Some [| "Het IBAN nummer is niet geldig." |]
            iban = iban
            bankData = {| bic = "" |} 
        }
}