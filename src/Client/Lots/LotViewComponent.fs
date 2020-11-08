module Client.Lots.LotViewComponent

open Fable.React
open Shared.Read
open Shared.Library
open Client.ClientStyle
open Client.ClientStyle.Helpers

let view (detail: Lot) =
    let ownerTypes (lotOwner: LotOwner) =
        match lotOwner.LotOwnerType with
        | LotOwnerType.Owner _ -> str "Persoon"
        | LotOwnerType.Organization o -> str ("Leverancier: " + (o.OrganizationTypeNames |> String.joinWith ", "))

    let ownerName (lotOwner: LotOwner) =
        match lotOwner.LotOwnerType with
        | LotOwnerType.Owner o        -> str (o.FullName ())
        | LotOwnerType.Organization o -> str o.Name

    let isResident (lotOwner: LotOwner) =
        match lotOwner.LotOwnerType with
        | LotOwnerType.Owner o        -> str (if o.IsResident then "Ja" else "Nee")
        | LotOwnerType.Organization _ -> str ""

    let isLegalRepresentative (lotOwner: LotOwner) =
        if lotOwner.LotOwnerRole = LegalRepresentative then str "Ja" else str "Nee"

    div [] [
        yield
            fieldset [] [
                yield legend [] [ h3 [] [ str "Algemeen" ] ]
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
                        legend [] [ h3 [] [ str "Eigenaar(s)" ] ]

                    yield
                        table [ classes [ Bootstrap.table; Bootstrap.tableStriped; Bootstrap.tableHover ] ] [
                            thead [] [
                                tr [] [
                                    th [] [ str "Type(s)" ]
                                    th [] [ str "Naam" ]
                                    th [] [ str "Inwoner" ]
                                    th [] [ str "Begindatum" ]
                                    th [] [ str "Einddatum" ]
                                    th [] [ str "Stemhouder" ]
                                ]
                            ]
                            tbody [] [
                                yield! owners |> List.sortBy (fun owner -> owner.EndDate, owner.StartDate) |> List.map (fun owner ->
                                    tr [] [
                                        td [] [ ownerTypes owner ]
                                        td [] [ ownerName owner ]
                                        td [] [ isResident owner ]
                                        td [] [ str (owner.StartDate.ToString("dd/MM/yyyy")) ]
                                        td [] [ str (owner.EndDate |> Option.either (fun ed -> ed.ToString("dd/MM/yyyy")) "") ]
                                        td [] [ isLegalRepresentative owner ]
                                    ]
                                )
                            ]
                        ]
                ]
    ]

let render =
    FunctionComponent.Of ((fun (props: {| Lot: Lot |}) -> view props.Lot), memoizeWith = memoEqualsButFunctions)