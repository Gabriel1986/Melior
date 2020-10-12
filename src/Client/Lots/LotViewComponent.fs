module Client.Lots.LotViewComponent

open Fable.React
open Fable.React.Props
open Shared.Read
open Shared.Library
open Client.ClientStyle
open Client.ClientStyle.Helpers

let view (detail: Lot) =
    let ownerTypes lotOwner =
        match lotOwner with
        | LotOwner.Owner _ -> str "Persoon"
        | LotOwner.Organization o -> str ("Leverancier: " + (o.OrganizationTypeNames |> String.JoinWith ", "))

    let ownerName lotOwner =
        match lotOwner with
        | LotOwner.Owner o        -> str (o.FullName ())
        | LotOwner.Organization o -> str o.Name

    let isResident lotOwner =
        match lotOwner with
        | LotOwner.Owner o        -> str (if o.IsResident then "Ja" else "Nee")
        | LotOwner.Organization _ -> str ""

    let isLegalRepresentative (_, lotOwnerRole: LotOwnerRole) =
        if lotOwnerRole = LegalRepresentative then str "Ja" else str "Nee"

    div [] [
        yield
            fieldset [] [
                yield legend [] [ h2 [] [ str "Algemeen" ] ]
                yield readonlyFormElement "Code" detail.Code
                yield readonlyFormElement "Type" (string detail.LotType)
                yield readonlyFormElement "Quotiteit" (detail.Share |> Option.map string |> Option.defaultValue "")
                yield readonlyFormElement "Verdieping" (detail.Floor |> Option.map string |> Option.defaultValue "")

                match detail.Description with
                | Some description ->
                    yield readonlyFormElement "Omschrijving" description
                | None ->
                    ()
            ]

        match detail.Owners with
        | [] ->
            ()
        | owners ->
            yield
                fieldset [] [
                    yield
                        legend [] [ h2 [] [ str "Eigenaar(s)" ] ]

                    yield
                        table [ classes [ Bootstrap.table; Bootstrap.tableStriped; Bootstrap.tableHover ] ] [
                            thead [] [
                                tr [] [
                                    th [] [ str "Type(s)" ]
                                    th [] [ str "Naam" ]
                                    th [] [ str "Inwoner" ]
                                    th [] [ str "Stemhouder" ]
                                ]
                            ]
                            tbody [] [
                                yield! owners |> List.map (fun owner ->
                                    tr [] [
                                        td [] [ ownerTypes (fst owner) ]
                                        td [] [ ownerName (fst owner) ]
                                        td [] [ isResident (fst owner) ]
                                        td [] [ isLegalRepresentative owner ]
                                    ]
                                )
                            ]
                        ]
                ]
    ]

let render =
    FunctionComponent.Of ((fun (props: {| Lot: Lot |}) -> view props.Lot), memoizeWith = memoEqualsButFunctions)