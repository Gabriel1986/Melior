module Client.Organizations.OrganizationViewComponent

open System
open Fable.React
open Shared.Read
open Client.ClientStyle.Helpers

let view (organization: Organization) =
    div [] [
        yield readonlyFormElement "Naam" organization.Name
        yield readonlyFormElement "Ondernemingsnummer" (string organization.OrganizationNumber)
        yield readonlyFormElement "Types" (organization.OrganizationTypes |> List.map (fun ot -> ot.Name) |> (fun result -> String.Join(", ", result)))
        yield readonlyFormElement "Adres" (string organization.Address)
        yield readonlyFormElement "Tel." (defaultArg organization.MainTelephoneNumber "")
        yield readonlyFormElement "Tel. commentaar" (defaultArg organization.MainTelephoneNumberComment "")

        yield readonlyFormElement "E-mail" (defaultArg organization.MainEmailAddress "")
        yield readonlyFormElement "E-mail commentaar" (defaultArg organization.MainEmailAddressComment "")

        yield! organization.OtherContactMethods |> List.map (fun cm -> readonlyFormElement cm.Description cm.Value)

        if organization.ContactPersons.Length > 0 then
            yield fieldset [] [
                yield legend [] [ h2 [] [ str "Contactpersonen" ] ]
                yield! organization.ContactPersons |> List.map (fun cp -> ContactPersonViewComponent.render {| ContactPerson = cp |})
            ]
    ]

let render =
    FunctionComponent.Of ((fun (props: {| Organization: Organization |}) -> view props.Organization), memoizeWith = memoEqualsButFunctions)