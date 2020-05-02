module Client.Components.SimpleLotComponent

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Domain
open Client
open Client.ClientStyle
open Client.SortableTable

type State = {
    Lots: LotListItem list option
    FatalException: bool
}

type Msg =
    | LotsLoaded of LotListItem list
    | RemotingException of exn
    | NavigateToLot of Guid

type SortableLotListItemAttribute =
    | BuildingCode
    | Code
    | LotType
    | Floor
    | Description
    | IsActive
    member me.ToString' () =
        match me with
        | BuildingCode -> "Gebouw"
        | Code -> "Code"
        | LotType -> "Type"
        | Floor -> "Verdieping"
        | Description -> "Omschrijving"
        | IsActive -> "Actief"
    member me.StringValueOf': LotListItem -> string =
        match me with
        | BuildingCode -> (fun li -> li.Building.Code)
        | Code -> (fun li -> li.Code)
        | LotType -> (fun li -> string li.LotType)
        | Floor -> (fun li -> string li.Floor)
        | Description -> (fun li -> li.Description |> Option.defaultValue "")
        | IsActive -> (fun li -> if li.IsActive then "Ja" else "Nee")
    member me.Compare': LotListItem -> LotListItem -> int =
        match me with
        | Floor -> 
            fun li otherLi -> li.Floor - otherLi.Floor
        | IsActive -> 
            fun li otherLi ->
                if li.IsActive = otherLi.IsActive then 0 
                elif li.IsActive && not otherLi.IsActive then 1 else -1
        | _     -> 
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ BuildingCode; Code;  LotType; Floor; Description; IsActive ]
    interface ISortableAttribute<LotListItem> with
        member me.ToString = me.ToString'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi

type SimpleLotComponentProps = {| Key: string; GetLots: unit -> Async<LotListItem list> |}

let init (props: SimpleLotComponentProps) =
    { Lots = None; FatalException = false },
        Cmd.OfAsync.either
            props.GetLots ()
            LotsLoaded
            RemotingException

let update (msg: Msg) (currentState: State): State * Cmd<Msg> =
    match msg with
    | LotsLoaded lots ->
        { currentState with Lots = Some lots }, Cmd.none
    | RemotingException ex ->
        { currentState with FatalException = true }, Cmd.none
    | NavigateToLot lotId ->
        currentState, Routing.navigateToPage (Routing.Page.LotDetails lotId)

let view (currentState: State) (dispatch: Msg -> unit) =
    fieldset [] [
        legend [] [ h2 [] [ str "Kavels" ] ]
        if currentState.FatalException 
        then div [] [ str "Er is iets misgelopen bij het ophalen van uw gegevens." ]
        else 
            SortableTable.render
                {|
                    ListItems = currentState.Lots |> Option.defaultValue []
                    DisplayAttributes = SortableLotListItemAttribute.All
                    ExtraColumnHeaders = []
                    ExtraColumns = (fun lot -> seq {
                        td [] [ 
                            button [ 
                                Class (sprintf "%s %s" Bootstrap.btn Bootstrap.btnLink)
                                OnClick (fun _ -> NavigateToLot lot.LotId |> dispatch) 
                            ] [ 
                                i [ Class (sprintf "%s %s" FontAwesome.fa FontAwesome.faExternalLinkAlt) ] []
                            ] 
                        ]
                    })
                    Key = "SimpleLotsTable"
                |}
    ]

let render (props: SimpleLotComponentProps) =
    React.elmishComponent("SimpleLotComponent", init props, update, view, props.Key)