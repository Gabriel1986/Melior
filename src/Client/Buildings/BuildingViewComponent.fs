module Client.Buildings.BuildingViewComponent

open System
open Fable.React
open Shared.Read
open Client.Components
open Client.ClientStyle.Helpers

let view (detail: Building) =
    div [] [
        fieldset [] [
            yield legend [] [ h2 [] [ str "Algemeen" ] ]
            yield readonlyFormElement "Code" detail.Code
            yield readonlyFormElement "Naam" detail.Name
            yield readonlyFormElement "Straat + nr." (defaultArg detail.Address.Street "")
            yield readonlyFormElement "Postcode" (defaultArg detail.Address.ZipCode "")
            yield readonlyFormElement "Woonplaats" (defaultArg detail.Address.Town "")
            
            match detail.Address.Country with 
            | Some country when country <> "België" -> 
                yield readonlyFormElement "Land" country
            | _ -> 
                ()
            
            yield readonlyFormElement "Bouwjaar" (detail.YearOfConstruction |> Option.map string |> Option.defaultValue "")
            yield readonlyFormElement "Opleveringsjaar" (detail.YearOfDelivery |> Option.map string |> Option.defaultValue "")

            match detail.OrganizationNumber with
            | Some number ->
                yield readonlyFormElement "Ondernemingsnummer" number
            | None ->
                ()

            match detail.Remarks with
            | Some remarks ->
                yield readonlyFormElement "Opmerkingen" remarks
            | None ->
                ()

            match detail.GeneralMeetingPeriod with
            | Some period ->
                let currentYear = DateTime.Today.Year
                let from = new DateTime(currentYear, period.FromMonth, period.FromDay)
                let until = new DateTime(currentYear, period.UntilMonth, period.UntilDay)
                yield readonlyFormElement "Periode algemene vergadering: " (sprintf "Tussen %s en %s" (from.ToString("dd/MM")) (until.ToString("dd/MM")))
            | _ ->
                ()

            yield readonlyFormElement "Actief" (if detail.IsActive then "Ja" else "Nee")
        ]
        match detail.Concierge with
        | Some concierge ->
            let person, isOwner = 
                match concierge with 
                | Concierge.Owner owner -> owner.Person, true 
                | Concierge.NonOwner person -> person, false

            fieldset [] [
                legend [] [ h2 [] [ str "Concierge" ] ]
                readonlyFormElement "Eigenaar?" (if isOwner then "Ja" else "Nee")
                PersonViewComponent.render {| Person = person; WithAddresses = false |}
            ]
        | None ->
            null
        match detail.Syndic with
        | Some syndic ->
            let person, syndicType = 
                match syndic with
                | Syndic.Owner owner -> owner.Person, "Eigenaar"
                | Syndic.ProfessionalSyndic pro -> pro.Person, "Professionele syndicus"
                | Syndic.Other person -> person, "Andere"

            fieldset [] [
                legend [] [ h2 [] [ str "Syndicus" ] ]
                readonlyFormElement "Type" syndicType
                PersonViewComponent.render {| Person = person; WithAddresses = true |}
            ]
        | None ->
            null
    ]

let render =
    FunctionComponent.Of ((fun (props: {| Building: Building |}) -> view props.Building), memoizeWith = memoEqualsButFunctions)