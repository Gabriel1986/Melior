module Client.IbanValidator

open Fable.Core

type IIbanValidator =
    abstract isValid: iban: string -> bool
    abstract toBBAN: iban: string -> separator: string -> string
    abstract fromBBAN: countryCode: string -> bban: string -> string
    abstract isValidBBAN: countryCode: string -> bban: string -> bool
    abstract printFormat: iban: string -> separator: string -> string
    abstract electronicFormat: iban: string -> string

[<Import("*", from="iban")>]
let ibanValidator: IIbanValidator = jsNative

let validateIban (iban: string): bool =
    ibanValidator.isValid(iban)