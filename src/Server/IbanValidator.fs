module Server.IbanValidator

open IbanNet

let private ibanValidator = new IbanValidator()

let validateIban (iban: string): bool = 
    let result = ibanValidator.Validate(iban)
    result.IsValid