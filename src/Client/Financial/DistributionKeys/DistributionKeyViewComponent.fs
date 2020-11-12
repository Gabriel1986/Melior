module Client.Financial.DistributionKeys.DistributionKeyViewComponent

open Fable.React
open Fable.React.Props
open Shared.Read
open Client
open Client.ClientStyle.Helpers
open Client.Routing
open Client.SortableTable
open Types

type SortableLotListItemAttribute =
    | OwnerName
    | Code
    | LotType
    | Floor
    | Share
    override me.ToString () =
        match me with
        | OwnerName -> "Eigenaar"
        | Code -> "Code"
        | LotType -> "Type"
        | Floor -> "Verd."
        | Share -> "Quot."
    member me.ReactElementFor': LotListItem -> ReactElement =
        match me with
        | Code -> 
            (fun li ->
                str (me.StringValueOf' li)
                |> wrapInLink (Page.LotDetails { BuildingId = li.BuildingId; DetailId = li.LotId }))
        | OwnerName -> 
            (fun li ->
                match li.LegalRepresentative with
                | None -> 
                    str ""
                | Some legalRepresentative ->
                    span [] [
                        let page =
                            match legalRepresentative with
                            | Owner owner -> 
                                Page.OwnerDetails { BuildingId = li.BuildingId; DetailId = owner.PersonId }
                            | Organization org -> 
                                Page.OrganizationDetails { BuildingId = li.BuildingId; DetailId = org.OrganizationId }
                        
                        str (me.StringValueOf' li)
                        |> wrapInLink page
                    ])
        | x -> (fun li -> str (x.StringValueOf' li))
    member me.ExtraHeaderAttributes': IHTMLProp seq =
        match me with
        | Code -> seq { Style [ Width "200px" ] }
        | Share -> seq { Style [ Width "100px" ]; Title "Quotiteit / aandeel" }
        | Floor -> seq { Style [ Width "100px" ] }
        | _ -> Seq.empty
    member me.StringValueOf': LotListItem -> string =
        match me with
        | OwnerName -> (fun li ->
            match li.LegalRepresentative with 
            | Some (LotOwnerListItem.Owner o) -> o.Name 
            | Some (LotOwnerListItem.Organization o) -> o.Name
            | None -> "")
        | Code -> (fun li -> li.Code)
        | LotType -> (fun li -> string li.LotType)
        | Floor -> (fun li -> string li.Floor)
        | Share -> (fun li -> li.Share |> Option.map string |> Option.defaultValue "")
    member me.Compare': LotListItem -> LotListItem -> int =
        match me with
        | Floor -> 
            fun li otherLi -> (defaultArg li.Floor -1000) - (defaultArg otherLi.Floor -1000)
        | Share ->
            fun li otherLi -> (defaultArg li.Share -1000) - (defaultArg otherLi.Share -1000)
        | _     -> 
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ LotType; Code; Share; OwnerName; Floor ]
    interface ISortableAttribute<LotListItem> with
        member me.ReactElementFor = me.ReactElementFor'
        member me.ExtraHeaderAttributes = me.ExtraHeaderAttributes'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true

type Props = {|
    DistributionKey: DistributionKeyModel
    AllLots: LotListItem list
|}

let renderLotSelectionList (list: LotListItem list) =
    SortableTable.render 
        {|
            ListItems = list
            DisplayAttributes = SortableLotListItemAttribute.All
            IsSelected = None
            OnSelect = None
            IsEditable = None
            OnEdit = None
            IsDeletable = None
            OnDelete = None
            Key = "LotSelectionList"
        |}

let view (props: Props) =
    let detail = props.DistributionKey
    div [] [
        fieldset [] [
            yield legend [] [ h2 [] [ str "Algemeen" ] ]
            yield readonlyFormElement "Naam" detail.Name
            yield readonlyFormElement "Verdeling" (string detail.DistributionType)
        ]            
        fieldset [] [            
            legend [] [ 
                h2 [] [ str "Kavels" ]
            ]
            renderLotSelectionList (detail.MatchingLots)
        ]
    ]

let render =
    FunctionComponent.Of ((fun (props: Props) -> view props), memoizeWith = memoEqualsButFunctions)