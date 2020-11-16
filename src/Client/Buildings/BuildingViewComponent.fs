module Client.Buildings.BuildingViewComponent

open System
open Fable.React
open Fable.React.Props
open Fable.DateFunctions
open Shared.Read
open Client.Components
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Organizations
open Shared.MediaLibrary

type Props = {|
    Building: Building
    OnEditSyndic: unit -> unit
    OnDeleteSyndic: unit -> unit
    OnEditConcierge: unit -> unit
    OnDeleteConcierge: unit -> unit 
|}

let view (props: Props) =
    let showDefaultImage = Hooks.useState props.Building.PictureId.IsNone
    let detail = props.Building
    div [] [
        fieldset [] [
            legend [] [ str "Algemeen" ]
            div [ Class Bootstrap.dFlex ] [
                div [] [
                    match props.Building.PictureId with
                    | Some pictureId when not (showDefaultImage.current) ->
                        img [
                            Src (Client.Upload.downloadUri Partitions.BuildingImages pictureId) 
                            Alt "Building image"
                            Style [ Height "250px"; Border "2px black solid" ]
                            OnError (fun _ -> showDefaultImage.update(fun _ -> true))
                        ]
                    | _ ->
                        img [
                            Src "https://i.ibb.co/rQnJ0hn/architecture-768432-640.jpg"
                            Alt "Building image"
                            Style [ Height "250px"; Border "2px black solid" ]
                        ]
                ]
                div [ classes [ Bootstrap.flexGrow1; Bootstrap.col] ] [
                        yield readonlyFormElement "Naam" detail.Name
                        yield readonlyFormElement "Code" detail.Code

                        match detail.OrganizationNumber with
                        | Some number ->
                            yield readonlyFormElement "Ondernemingsnummer" number
                        | None ->
                            ()

                        yield readonlyFormElement "Adres" (string detail.Address)

                        yield readonlyFormElement "Bouwjaar" (detail.YearOfConstruction |> Option.map string |> Option.defaultValue "")
                        yield readonlyFormElement "Opleveringsjaar" (detail.YearOfDelivery |> Option.map string |> Option.defaultValue "")

            
                        //match detail.Remarks with
                        //| Some remarks ->
                        //    yield readonlyFormElement "Opmerkingen" remarks
                        //| None ->
                        //    ()

                        match detail.GeneralMeetingPeriod with
                        | Some period ->
                            let currentYear = DateTime.Today.Year
                            let from = new DateTime(currentYear, period.FromMonth, period.FromDay)
                            let until = new DateTime(currentYear, period.UntilMonth, period.UntilDay)
                            yield readonlyFormElement "Periode algemene vergadering " (sprintf "%s tot %s" (from.Format("dd MMMM", DateTime.Locales.Dutch)) (until.Format("dd MMMM", DateTime.Locales.Dutch)))
                        | _ ->
                            ()

                        yield!
                            detail.BankAccounts
                            |> List.mapi (fun i bankAccount -> 
                                let bankAccountDescription =
                                    if detail.BankAccounts.Length > 1 then
                                        sprintf "Bankrekening %i" (i+1)
                                    else
                                        "Bankrekening"
                                readonlyFormElement bankAccountDescription (string bankAccount))
                    ]
                ]
        ]

        match detail.Concierge with
        | Some concierge ->
            let person, isOwner = 
                match concierge with 
                | Concierge.Owner owner -> owner.Person, true 
                | Concierge.NonOwner person -> person, false

            fieldset [ Class Bootstrap.mt2 ] [
                legend [] [
                    div [ Class Bootstrap.formInline ] [
                        div [ Class Bootstrap.formGroup ] [
                            span [ Class Bootstrap.mr2 ] [ str "Concierge" ]
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
                PersonViewComponent.render {| Person = person; ShowAddresses = true; ShowBankAccounts = true |}
            ]
        | None ->
            null
        match detail.Syndic with
        | Some syndic ->
            let viewComponent, syndicType = 
                match syndic with
                | Syndic.Owner owner -> PersonViewComponent.render {| Person = owner.Person; ShowAddresses = true; ShowBankAccounts = true |}, "Eigenaar"
                | Syndic.ProfessionalSyndic pro -> OrganizationViewComponent.render {| Organization = pro.Organization |}, "Professionele syndicus"
                | Syndic.Other person -> PersonViewComponent.render {| Person = person; ShowAddresses = true; ShowBankAccounts = true |}, "Andere"

            fieldset [ Class Bootstrap.mt2 ] [
                legend [] [ 
                    div [ Class Bootstrap.formInline ] [
                        div [ Class Bootstrap.formGroup ] [
                            span [ Class Bootstrap.mr2 ] [ str "Syndicus" ]
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