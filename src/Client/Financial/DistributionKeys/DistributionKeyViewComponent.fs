module Client.Financial.DistributionKeys.DistributionKeyViewComponent

open Fable.React
open Shared.Read
open Client
open Client.ClientStyle.Helpers
open Types

type Props = {|
    DistributionKey: DistributionKeyModel
    AllLots: LotListItem list
|}

let renderLotSelectionList (list: LotListItem list) (selected: LotListItem list) =
    SortableTable.render 
        {|
            ListItems = list
            DisplayAttributes = Client.Lots.LotsPage.SortableLotListItemAttribute.All
            IsSelected = Some (fun listItem -> selected |> List.contains listItem)
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
            renderLotSelectionList (props.AllLots) (detail.MatchingLots)
        ]
    ]

let render =
    FunctionComponent.Of ((fun (props: Props) -> view props), memoizeWith = memoEqualsButFunctions)