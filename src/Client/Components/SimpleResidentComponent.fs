module Client.Components.SimpleResidentComponent

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
    Residents: ResidentListItem list option
    FatalException: bool
}

type Msg =
    | ResidentsLoaded of ResidentListItem list
    | RemotingException of exn
    | NavigateToResident of Guid

type SortableResidentListItemAttribute =
    | FirstName
    | LastName
    | IsActive
    | MovedInDate
    | MovedOutDate
    member me.ToString' () =
        match me with
        | FirstName -> "Voornaam"
        | LastName -> "Achternaam"
        | IsActive -> "Actief"
        | MovedInDate -> "Datum intrek"
        | MovedOutDate -> "Datum vertrek"
    member me.StringValueOf': ResidentListItem -> string =
        match me with
        | FirstName -> (fun li -> string li.FirstName)
        | LastName -> (fun li -> string li.LastName)
        | IsActive -> (fun li -> if li.IsActive then "Ja" else "Nee")
        | MovedInDate -> (fun li -> li.MovedInDate.ToString("dd-MM-yyyy"))
        | MovedOutDate -> (fun li -> 
            li.MovedOutDate 
            |> Option.map (fun d -> d.ToString("dd-MM-yyyy")) 
            |> Option.defaultValue "")
    member me.Compare': ResidentListItem -> ResidentListItem -> int =
        match me with
        | IsActive -> 
            fun li otherLi ->
                if li.IsActive = otherLi.IsActive then 0 
                elif li.IsActive && not otherLi.IsActive then 1 else -1
        | MovedInDate -> 
            fun li otherLi -> li.MovedInDate.CompareTo(otherLi.MovedInDate)
        | MovedOutDate ->
            fun li otherLi ->
                match li.MovedOutDate, otherLi.MovedOutDate with
                | Some d1, Some d2 -> d1.CompareTo(d2)
                | Some _, None -> 1
                | None, Some _ -> -1
                | None, None    -> 0
        | _     -> 
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ FirstName; LastName; MovedInDate; MovedOutDate; IsActive ]
    interface ISortableAttribute<ResidentListItem> with
        member me.ToString = me.ToString'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi

type SimpleResidentComponentProps = {| Key: string; GetResidents: unit -> Async<ResidentListItem list> |}

let init (props: SimpleResidentComponentProps) =
    { Residents = None; FatalException = false },
        Cmd.OfAsync.either
            props.GetResidents ()
            ResidentsLoaded
            RemotingException

let update (msg: Msg) (currentState: State): State * Cmd<Msg> =
    match msg with
    | ResidentsLoaded residents ->
        { currentState with Residents = Some residents }, Cmd.none
    | RemotingException ex ->
        { currentState with FatalException = true }, Cmd.none
    | NavigateToResident residentId ->
        currentState, Routing.navigateToPage (Routing.Page.ResidentDetails residentId)

let view (currentState: State) (dispatch: Msg -> unit) =
    fieldset [] [
        legend [] [ h2 [] [ str "Bewoners" ] ]
        if currentState.FatalException 
        then div [] [ str "Er is iets misgelopen bij het ophalen van uw gegevens." ]
        else 
            SortableTable.render
                {|
                    ListItems = currentState.Residents |> Option.defaultValue []
                    DisplayAttributes = SortableResidentListItemAttribute.All
                    ExtraColumnHeaders = [
                        th [] []
                    ]
                    ExtraColumns = (fun resident -> seq {
                        td [] [ 
                            button [ 
                                Class (sprintf "%s %s" Bootstrap.btn Bootstrap.btnLink)
                                OnClick (fun _ -> NavigateToResident resident.ResidentId |> dispatch) 
                            ] [ 
                                i [ Class (sprintf "%s %s" FontAwesome.fa FontAwesome.faExternalLinkAlt) ] []
                            ] 
                        ]
                    })
                    Key = "SimpleResidentsTable"
                |}
    ]

let render (props: SimpleResidentComponentProps) =
    React.elmishComponent("SimpleResidentComponent", init props, update, view, props.Key)