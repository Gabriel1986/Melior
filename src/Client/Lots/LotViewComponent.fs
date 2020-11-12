module Client.Lots.LotViewComponent

open Fable.React
open Fable.React.Props
open Shared.Read
open Shared.Library
open Client
open Client.ClientStyle.Helpers
open Client.Routing
open Client.SortableTable

type OwnerSortableAttribute =
    | Types
    | Name
    | IsResident
    | StartDate
    | EndDate
    | IsLegalRepresentative
    override me.ToString () =
        match me with
        | Types -> "Type(s)"
        | Name -> "Naam"
        | IsResident -> "Inwoner"
        | StartDate -> "Begindatum"
        | EndDate -> "Einddatum"
        | IsLegalRepresentative -> "Stemhouder"
    member me.ReactElementFor': LotOwner -> ReactElement =
        match me with
        | Name ->
            (fun li ->
                let page =
                    match li.LotOwnerType with
                    | LotOwnerType.Owner owner ->
                        Page.OwnerDetails { BuildingId = owner.BuildingId; DetailId = owner.PersonId }
                    | LotOwnerType.Organization org ->
                        Page.OrganizationDetails { BuildingId = org.BuildingId.Value; DetailId = org.OrganizationId }

                str (me.StringValueOf' li)
                |> wrapInLink page)
        | _ -> fun li -> str (me.StringValueOf' li) 
    member me.ExtraHeaderAttributes': IHTMLProp seq =
        match me with
        | IsResident -> seq { Style [ Width "120px" ] }
        | StartDate -> seq { Style [ Width "150px" ] }
        | EndDate -> seq { Style [ Width "150px" ] }
        | IsLegalRepresentative -> seq { Style [ Width "120px" ] }
        | _ -> Seq.empty
    member me.StringValueOf': LotOwner -> string =
        match me with
        | Types ->
            (fun li -> 
                match li.LotOwnerType with
                | LotOwnerType.Owner _ -> "Persoon"
                | LotOwnerType.Organization o -> "Leverancier: " + (o.OrganizationTypeNames |> String.joinWith ", "))
        | Name ->
            (fun li ->
                match li.LotOwnerType with
                | LotOwnerType.Owner o        -> o.FullName ()
                | LotOwnerType.Organization o -> o.Name)
        | IsResident ->
            (fun li ->
                match li.LotOwnerType with
                | LotOwnerType.Owner o        -> if o.IsResident then "Ja" else "Nee"
                | LotOwnerType.Organization _ -> "")
        | StartDate ->
            (fun li -> li.StartDate.ToString("dd/MM/yyyy"))
        | EndDate ->
            (fun li -> li.EndDate |> Option.either (fun ed -> ed.ToString("dd/MM/yyyy")) "")
        | IsLegalRepresentative ->
            (fun li -> if li.LotOwnerRole = LegalRepresentative then "Ja" else "Nee")
    member me.Compare': LotOwner -> LotOwner -> int =
        match me with
        | StartDate ->
            fun li otherLi -> li.StartDate.CompareTo(otherLi.StartDate)
        | EndDate ->
            fun li otherLi ->
                match li.EndDate, otherLi.EndDate with
                | Some endDate, Some otherEndDate -> endDate.CompareTo(otherEndDate)
                | None, Some _ -> -1
                | Some _, None -> 1
                | None, None -> 0
        | _ -> fun li otherLi -> me.StringValueOf'(li).CompareTo(me.StringValueOf'(otherLi))
    static member All = [ Types; Name; IsResident; StartDate; EndDate; IsLegalRepresentative ]
    interface ISortableAttribute<LotOwner> with
        member me.ReactElementFor = me.ReactElementFor'
        member me.ExtraHeaderAttributes = me.ExtraHeaderAttributes'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true

let view (detail: Lot) =
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
                    legend [] [ 
                        h3 [] [ str "Eigenaar(s)" ] 
                    ]

                    SortableTable.render 
                        {| 
                            ListItems = detail.Owners
                            DisplayAttributes = OwnerSortableAttribute.All
                            IsSelected = None
                            OnSelect = None
                            IsEditable = None
                            OnEdit = None
                            IsDeletable = None
                            OnDelete = None
                            Key = "BuildingsPageTable"
                        |}
                ]
    ]

let render =
    FunctionComponent.Of ((fun (props: {| Lot: Lot |}) -> view props.Lot), memoizeWith = memoEqualsButFunctions)