module Client.Lots.LotViewComponent

open Fable.React
open Shared.Read
open Client.ClientStyle.Helpers
open Client.Organizations
open Client.Components

let renderOwner (owner: Owner) =
    PersonViewComponent.render {| Person = owner.Person; WithAddresses = true |}

let renderOrganization (organization: Organization) =
    OrganizationViewComponent.render {| Organization = organization |}

let view (detail: Lot) =
    div [] [
        yield
            fieldset [] [
                yield legend [] [ h2 [] [ str "Algemeen" ] ]
                yield readonlyFormElement "Code" detail.Code
                yield readonlyFormElement "Type" (string detail.LotType)
                yield readonlyFormElement "Verdieping" (detail.Floor |> Option.map string |> Option.defaultValue "")
                yield readonlyFormElement "Oppervlakte" (detail.Surface |> Option.map string |> Option.defaultValue "")

                match detail.Description with
                | Some description ->
                    yield readonlyFormElement "Omschrijving" description
                | None ->
                    ()
            ]

        match detail.CurrentOwner with
        | Some owner ->
            yield
                fieldset [] [
                    yield
                        legend [] [ h2 [] [ str "Eigenaar" ] ]

                    yield
                        match owner with 
                        | LotOwner.Owner _ -> readonlyFormElement "Type" "Eigenaar"
                        | LotOwner.Organization _ -> readonlyFormElement "Type" "Organisatie"
                    yield
                        match owner with
                        | LotOwner.Owner owner -> renderOwner owner
                        | LotOwner.Organization organization -> renderOrganization organization
                ]
        | None ->
            ()


    ]

let render =
    FunctionComponent.Of ((fun (props: {| Lot: Lot |}) -> view props.Lot), memoizeWith = memoEqualsButFunctions)