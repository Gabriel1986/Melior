module Client.Components.PersonViewComponent

open Feliz
open Fable.React
open Shared.Read
open Client.ClientStyle.Helpers

let private renderOtherAddress (other: OtherAddress) =
    readonlyFormElement' other.Name (string other.Address) other.Description

let private renderAddressesFor (person: Person) =
    let contactAdres = 
        match person.ContactAddress with
        | MainAddress -> "Zelfde als hoofdadres"
        | ContactAddress a -> (string a)

    [
        yield readonlyFormElement "Hoofdadres" (string person.MainAddress)
        yield readonlyFormElement "Contactadres" contactAdres
        yield! person.OtherAddresses |> List.map renderOtherAddress
    ]

let view (showAddresses: bool) (showBankAccounts: bool) (person: Person) =
    [
        yield readonlyFormElement "Naam" (person.FullName ())

        if showAddresses then yield! renderAddressesFor person

        yield readonlyFormElement "Tel." (defaultArg person.MainTelephoneNumber "")
        yield readonlyFormElement "Tel. commentaar" (defaultArg person.MainTelephoneNumberComment "")

        yield readonlyFormElement "E-mail" (defaultArg person.MainEmailAddress "")
        yield readonlyFormElement "E-mail commentaar" (defaultArg person.MainEmailAddressComment "")

        yield! person.OtherContactMethods |> List.map (fun cm -> readonlyFormElement cm.Description cm.Value)

        if showBankAccounts then
            yield!
                person.BankAccounts
                |> List.mapi (fun i bankAccount -> readonlyFormElement (sprintf "Bankrekening %i" (i+1)) (string bankAccount))
    ]
    |> React.fragment

let render =
    FunctionComponent.Of ((fun (props: {| Person: Person; ShowAddresses: bool; ShowBankAccounts: bool |}) -> view props.ShowAddresses props.ShowBankAccounts props.Person), memoizeWith = memoEqualsButFunctions)