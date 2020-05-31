module Client.Organizations.OrganizationViewComponent

open Fable.React
open Shared.Read
open Client.ClientStyle.Helpers
open Client.Components

let private renderContactPerson (contactPerson: ContactPerson) =
    div [] [
        readonlyFormElement "Rol" contactPerson.RoleWithinOrganization
        PersonViewComponent.render {| Person = contactPerson.Person; WithAddresses = false |}
    ]

let private renderOtherContactPersonsFor (organization: Organization) =
    organization.OtherContactPersons |> List.map renderContactPerson

let view (organization: Organization) =
    div [] [
        yield readonlyFormElement "Naam" organization.Name
        yield readonlyFormElement "Ondernemingsnummer" (string organization.OrganizationNumber)
        yield readonlyFormElement "Type" (string organization.OrganizationType.Name)
        yield readonlyFormElement "Adres" (string organization.Address)

        yield 
            fieldset [] [
                yield legend [] [ h2 [] [ str "Hoofdcontactpersoon" ] ]
                yield renderContactPerson organization.MainContactPerson
            ]

        yield
            fieldset [] [
                yield legend [] [ h2 [] [ str "Andere contactpersonen" ] ]
                yield! renderOtherContactPersonsFor organization
            ]
    ]

let render =
    FunctionComponent.Of ((fun (props: {| Organization: Organization |}) -> view props.Organization), memoizeWith = memoEqualsButFunctions)