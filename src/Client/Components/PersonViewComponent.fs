module Client.Components.PersonViewComponent

open Feliz
open Fable.React
open Shared.Read
open Client.ClientStyle.Helpers

let private renderAddressesFor (person: Person) =
    let contactAdres = 
        match person.ContactAddress with
        | MainAddress -> "Zelfde als hoofdadres"
        | ContactAddress a -> (string a)

    [
        yield readonlyFormElement "Hoofdadres" (string person.MainAddress)
        yield readonlyFormElement "Contactadres" contactAdres
        yield! person.OtherAddresses |> List.mapi (fun index otherAddress ->
            let otherAddressDescription =
                if person.OtherAddresses.Length > 1 then
                    sprintf "Ander adres %i" (index+1)
                else
                    "Ander adres"
            readonlyFormElement otherAddressDescription (sprintf "%s - %A" otherAddress.Description otherAddress.Address))
    ]

let private view (showAddresses: bool) (showBankAccounts: bool) (person: Person) =
    [
        yield readonlyFormElement "Naam" (person.FullName ())

        if showAddresses then yield! renderAddressesFor person

        yield readonlyFormElement "Tel." (defaultArg person.MainTelephoneNumber "")
        yield readonlyFormElement "Tel. commentaar" (defaultArg person.MainTelephoneNumberComment "")

        yield readonlyFormElement "E-mail" (defaultArg person.MainEmailAddress "")
        yield readonlyFormElement "E-mail commentaar" (defaultArg person.MainEmailAddressComment "")

        yield! person.OtherContactMethods |> List.mapi (fun index cm -> 
            let otherContactMethodDescription =
                if person.OtherContactMethods.Length > 1 then
                    sprintf "Ander contactmiddel %i" (index+1)
                else
                    "Ander contactmiddel"
            readonlyFormElement otherContactMethodDescription (sprintf "%s - %s" cm.Description cm.Value))

        if showBankAccounts then
            yield!
                person.BankAccounts
                |> List.mapi (fun i bankAccount -> 
                    let bankAccountDescription =
                        if person.BankAccounts.Length > 1 then
                            sprintf "Bankrekening %i" (i+1)
                        else
                            "Bankrekening"
                    readonlyFormElement bankAccountDescription (string bankAccount))
    ]
    |> React.fragment

let render =
    FunctionComponent.Of ((fun (props: {| Person: Person; ShowAddresses: bool; ShowBankAccounts: bool |}) -> view props.ShowAddresses props.ShowBankAccounts props.Person), memoizeWith = memoEqualsButFunctions)