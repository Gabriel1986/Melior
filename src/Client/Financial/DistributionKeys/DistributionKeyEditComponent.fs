module Client.Financial.DistributionKeys.DistributionKeyEditComponent

open Shared.Read
open Client
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Types

type Message =
    | NameChanged of string
    | DistributionTypeChanged of DistributionType
    | ToggleSelectLot of LotListItem

type State = {
    DistributionKey: DistributionKeyModel
    AllLots: LotListItem list
    Errors: (string * string) list
}

let init (distributionKey: DistributionKeyModel, allLots: LotListItem list) =
    { 
        DistributionKey = distributionKey
        AllLots = allLots
        Errors = []
    }, Cmd.none

let update (message: Message) (state: State): State * Cmd<Message> =
    let changeDistributionKey f =
        { state with DistributionKey = f state.DistributionKey }

    match message with
    | NameChanged x ->
        changeDistributionKey (fun dKey -> { dKey with Name = x }), Cmd.none
    | DistributionTypeChanged x ->
        changeDistributionKey (fun dKey -> { dKey with DistributionType = x }), Cmd.none
    | ToggleSelectLot x ->
        let lots =
            if state.DistributionKey.MatchingLots |> List.contains x
            then state.DistributionKey.MatchingLots |> List.filter (fun lot -> lot <> x)
            else x::state.DistributionKey.MatchingLots
        changeDistributionKey (fun dKey -> { dKey with MatchingLots = lots }), Cmd.none

let inColomn x = div [ Class Bootstrap.col ] [ x ]

let renderLotSelectionList (list: LotListItem list) dispatch (selected: LotListItem list) =
    SortableTable.render 
        {|
            ListItems = list
            DisplayAttributes = Client.Lots.LotsPage.SortableLotListItemAttribute.All
            IsSelected = Some (fun listItem -> selected |> List.contains listItem)
            OnSelect = Some (fun listItem -> ToggleSelectLot listItem |> dispatch)
            IsEditable = None
            OnEdit = None
            IsDeletable = None
            OnDelete = None
            Key = "LotSelectionList"
        |}

let view (state: State) (dispatch: Message -> unit) =
    let errorFor (path: string) =
        state.Errors |> List.tryPick (fun (p, error) -> if p = path then Some error else None)

    div [] [
        div [ Class Bootstrap.row ] [
            formGroup [ 
                Label "Naam"
                Input [ 
                    Type "text"
                    MaxLength 255.0
                    Helpers.valueOrDefault state.DistributionKey.Name
                    OnChange (fun e -> NameChanged e.Value |> dispatch)
                ] 
                FormError (errorFor (nameof state.DistributionKey.Name))
            ]
            |> inColomn
            formGroup [ 
                Label "Verdeling"
                Select {
                    Identifier = string state.DistributionKey.DistributionType
                    OnChanged = (fun newTypeString ->
                        let newType =
                            match newTypeString with
                            | s when s = string DistributionType.Shares -> 
                                DistributionType.Shares
                            | _                              -> 
                                DistributionType.EqualParts
                        DistributionTypeChanged newType |> dispatch
                    )
                    Options = [
                        {
                            Key = string DistributionType.Shares
                            Label = "Volgens aandelen"
                            IsSelected = state.DistributionKey.DistributionType = DistributionType.Shares
                        }
                        {
                            Key = string DistributionType.EqualParts
                            Label = "Volgens gelijke delen"
                            IsSelected = state.DistributionKey.DistributionType = DistributionType.EqualParts
                        }
                    ]
                }
            ]
            |> inColomn
        ]
        div [ Class Bootstrap.row ] [
            renderLotSelectionList state.AllLots dispatch state.DistributionKey.MatchingLots
            |> inColomn
        ]
    ]