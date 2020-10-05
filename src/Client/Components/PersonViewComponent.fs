module Client.Components.PersonViewComponent

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

let view (withAddresses: bool) (person: Person) =
    div [] [
        yield readonlyFormElement "Naam" (person.FullName ())

        if withAddresses then yield! renderAddressesFor person

        yield readonlyFormElement "Tel." (defaultArg person.MainTelephoneNumber "")
        yield readonlyFormElement "Tel. commentaar" (defaultArg person.MainTelephoneNumberComment "")

        yield readonlyFormElement "E-mail" (defaultArg person.MainEmailAddress "")
        yield readonlyFormElement "E-mail commentaar" (defaultArg person.MainEmailAddressComment "")

        yield! person.OtherContactMethods |> List.map (fun cm -> readonlyFormElement cm.Description cm.Value)

        yield!
            person.BankAccounts
            |> List.mapi (fun i bankAccount -> readonlyFormElement (sprintf "Bankrekening %i" (i+1)) (string bankAccount))

    ]

let render =
    FunctionComponent.Of ((fun (props: {| Person: Person; WithAddresses: bool |}) -> view props.WithAddresses props.Person), memoizeWith = memoEqualsButFunctions)