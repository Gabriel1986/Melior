module Client.Buildings.BuildingViewComponent

open System
open Fable.React
open Fable.React.Props
open Shared.Read
open Client.Components
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Organizations

type Props = {|
    Building: Building
    OnEditSyndic: unit -> unit
    OnDeleteSyndic: unit -> unit
    OnEditConcierge: unit -> unit
    OnDeleteConcierge: unit -> unit 
|}

let view (props: Props) =
    let detail = props.Building
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
        ]
        match detail.Concierge with
        | Some concierge ->
            let person, isOwner = 
                match concierge with 
                | Concierge.Owner owner -> owner.Person, true 
                | Concierge.NonOwner person -> person, false

            fieldset [] [
                legend [] [ 
                    div [ Class Bootstrap.formInline ] [
                        div [ Class Bootstrap.formGroup ] [
                            h2 [ Class Bootstrap.mr2 ] [ str "Concierge" ]
                            button [ 
                                classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary; Bootstrap.btnSm; Bootstrap.mr2 ]
                                OnClick (fun _ -> props.OnEditConcierge ()) 
                            ] [ i [ classes [ FontAwesome.fa; FontAwesome.faEdit ] ] [] ]
                            button [
                                classes [ Bootstrap.btn; Bootstrap.btnOutlineDanger; Bootstrap.btnSm ]
                                OnClick (fun _ -> props.OnDeleteConcierge ())
                            ] [ i [ classes [ FontAwesome.fa; FontAwesome.faTrash ] ] [] ]
                        ]
                    ]
                ]
                readonlyFormElement "Eigenaar?" (if isOwner then "Ja" else "Nee")
                PersonViewComponent.render {| Person = person; WithAddresses = false |}
            ]
        | None ->
            null
        match detail.Syndic with
        | Some syndic ->
            let viewComponent, syndicType = 
                match syndic with
                | Syndic.Owner owner -> PersonViewComponent.render {| Person = owner.Person; WithAddresses = true |}, "Eigenaar"
                | Syndic.ProfessionalSyndic pro -> OrganizationViewComponent.render {| Organization = pro.Organization |}, "Professionele syndicus"
                | Syndic.Other person -> PersonViewComponent.render {| Person = person; WithAddresses = true |}, "Andere"

            fieldset [] [
                legend [] [ 
                    div [ Class Bootstrap.formInline ] [
                        div [ Class Bootstrap.formGroup ] [
                            h2 [ Class Bootstrap.mr2 ] [ str "Syndicus" ] 
                            button [ 
                                classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary; Bootstrap.btnSm; Bootstrap.mr2 ]
                                OnClick (fun _ -> props.OnEditSyndic ()) 
                            ] [ i [ classes [ FontAwesome.fa; FontAwesome.faEdit ] ] [] ]
                            button [
                                classes [ Bootstrap.btn; Bootstrap.btnOutlineDanger; Bootstrap.btnSm ]
                                OnClick (fun _ -> props.OnDeleteSyndic ())
                            ] [ i [ classes [ FontAwesome.fa; FontAwesome.faTrash ] ] [] ]
                        ]
                    ]
                ]
                readonlyFormElement "Type" syndicType
                viewComponent
            ]
        | None ->
            null
    ]

let render =
    FunctionComponent.Of ((fun (props: Props) -> view props), memoizeWith = memoEqualsButFunctions)