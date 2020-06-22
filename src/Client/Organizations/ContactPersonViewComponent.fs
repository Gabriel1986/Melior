module Client.Organizations.ContactPersonViewComponent

open Fable.React
open Shared.Read
open Client.Components
open Client.ClientStyle.Helpers

let view (props: {| ContactPerson: ContactPerson |}) =
    div [] [
        readonlyFormElement "Rol" props.ContactPerson.RoleWithinOrganization
        PersonViewComponent.render {| Person = props.ContactPerson.Person; WithAddresses = false |}
    ]

let render =
    FunctionComponent.Of (view, memoizeWith = memoEqualsButFunctions)