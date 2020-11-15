module Client.Organizations.OrganizationViewComponent

open System
open Fable.React
open Shared.Read
open Client.ClientStyle.Helpers

let view (organization: Organization) =
    div [] [
        yield readonlyFormElement "Naam" organization.Name
        yield readonlyFormElement "Ondernemingsnummer" (organization.OrganizationNumber |> Option.defaultValue "")
        yield readonlyFormElement "BTW nummer" (organization.VatNumber |> Option.defaultValue "")
        yield readonlyFormElement "Types" (organization.OrganizationTypes |> List.map (fun ot -> ot.Name) |> (fun result -> String.Join(", ", result)))
        yield readonlyFormElement "Adres" (string organization.Address)
        yield readonlyFormElement "Tel." (defaultArg organization.MainTelephoneNumber "")
        yield readonlyFormElement "Tel. commentaar" (defaultArg organization.MainTelephoneNumberComment "")

        yield readonlyFormElement "E-mail" (defaultArg organization.MainEmailAddress "")
        yield readonlyFormElement "E-mail commentaar" (defaultArg organization.MainEmailAddressComment "")

        yield! organization.OtherContactMethods |> List.mapi (fun index cm ->
            let otherContactMethodDescription =
                if organization.OtherContactMethods.Length > 1 then
                    sprintf "Ander contactmiddel %i" (index+1)
                else
                    "Ander contactmiddel"
            readonlyFormElement otherContactMethodDescription (sprintf "%s - %s" cm.Description cm.Value))


        if organization.ContactPersons.Length > 0 then
            yield fieldset [] [
                yield legend [] [ h2 [] [ str "Contactpersonen" ] ]
                yield! organization.ContactPersons |> List.map (fun cp -> ContactPersonViewComponent.render {| ContactPerson = cp |})
            ]

        yield!
            organization.BankAccounts
            |> List.mapi (fun i bankAccount -> 
                let bankAccountDescription =
                    if organization.BankAccounts.Length > 1 then
                        sprintf "Bankrekening %i" (i+1)
                    else
                        "Bankrekening"
                readonlyFormElement bankAccountDescription (string bankAccount))
    ]

let render =
    FunctionComponent.Of ((fun (props: {| Organization: Organization |}) -> view props.Organization), memoizeWith = memoEqualsButFunctions)