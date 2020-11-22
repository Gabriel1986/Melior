module Client.Lots.LotViewComponent

open System
open Fable.React
open Fable.React.Props
open Elmish
open Feliz.ElmishComponents
open Shared.Read
open Shared.Library
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Routing
open Client.Components
open Client.Components.BasicModal

let view (detail: Lot) =
    let personViewState = Hooks.useState None

    [
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
                    legend [] [ 
                        h3 [] [ str "Eigenaar(s)" ] 
                    ]

                    table [ classes [ Bootstrap.table; Bootstrap.tableStriped; Bootstrap.tableHover ] ] [
                        thead [] [
                            tr [] [
                                th [] [ str "Naam" ]
                                th [ Style [ Width "150px" ] ] [ str "Begindatum" ]
                                th [ Style [ Width "150px" ] ] [ str "Einddatum" ]
                            ]
                        ]
                        tbody [] [
                            yield! owners
                            |> List.map (fun owner ->
                                [
                                    tr [ Key (string owner.LotOwnerId) ] [
                                        td [] [
                                            match owner.LotOwnerType with
                                            | LotOwnerType.Owner o -> 
                                                str (o.FullName ())
                                                |> wrapInLink (OwnerDetails { BuildingId = detail.BuildingId; DetailId = o.PersonId })
                                            | LotOwnerType.Organization o -> 
                                                str o.Name
                                                |> wrapInLink (OrganizationDetails { BuildingId = detail.BuildingId; DetailId = o.OrganizationId })
                                        ]
                                        td [] [
                                            str (owner.StartDate.ToString("dd/MM/yyyy"))
                                        ]
                                        td [] [
                                            match owner.EndDate with
                                            | Some endDate -> str (endDate.ToString("dd/MM/yyyy"))
                                            | None -> str ""
                                        ]
                                    ]
                                    match owner.Contacts with
                                    | [] -> null
                                    | contacts ->
                                        tr [] [
                                            td [ ColSpan 4 ] [
                                                h6 [ Class Bootstrap.ml4 ] [ str "Contactpersonen" ]
                                                ul [ classes [ Bootstrap.listGroup; Bootstrap.ml4 ] ] [
                                                    yield! contacts |> List.map (fun contact ->
                                                        li [ Key (string contact.PersonId); Class Bootstrap.listGroupItem ] [
                                                            match contact with
                                                            | LotOwnerContact.Owner o ->
                                                                str (sprintf "%s (eigenaar)" (o.FullName ()))
                                                                |> wrapInLink (OwnerDetails { BuildingId = detail.BuildingId; DetailId = contact.PersonId })
                                                            | LotOwnerContact.NonOwner p ->
                                                                a [ Href "javascript:void(0);"; OnClick (fun e -> e.preventDefault(); e.stopPropagation(); personViewState.update (Some p)) ] [
                                                                    str (p.FullName())
                                                                ]
                                                        ]
                                                    )
                                                ]
                                            ]
                                        ]
                                ]
                                |> fragment []
                            )
                        ]
                    ]
                ]
        match personViewState.current with
        | None -> ()
        | Some person ->
            yield BasicModal.render
                {|
                    ModalProps = [
                        IsOpen true
                        OnDismiss (fun () -> personViewState.update None)
                        Header [
                            HeaderProp.HasDismissButton true
                            HeaderProp.Title "Contactpersoon"
                        ]
                        Body [
                            PersonViewComponent.render
                                {|
                                    Person = person
                                    ShowAddresses = true
                                    ShowBankAccounts = false
                                |}
                        ]
                        Footer [
                            ShowDismissButton (Some "Sluiten")
                        ]
                    ]
                |}
    ]
    |> fragment []

let render =
    FunctionComponent.Of ((fun (props: {| Lot: Lot |}) -> view props.Lot), memoizeWith = memoEqualsButFunctions)