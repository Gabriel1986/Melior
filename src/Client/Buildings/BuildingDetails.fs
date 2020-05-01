module Client.Buildings.BuildingDetails

open System
open Elmish
open Fable
open Fable.React
open Feliz
open Feliz.ElmishComponents

open Shared.Domain
open Client

open Shared.Buildings

type Model = {
    BuildingId: Guid
    CurrentUser: CurrentUser
    State: State
    NotifyCreated: Building -> unit
    NotifyEdited:  Building -> unit
}
and State =
    | Loading
    | Viewing  of detail: Building
    | Editing  of detail: Building * isSaving: bool
    | Creating of detail: Building * isSaving: bool
    | BuildingNotFound
    | RemotingError of exn

type Msg =
    | View of Building option
    | RemotingError of exn
    | Save
    | ProcessSaveResult of Result<Building, InvariantError>

type DetailsProps = {|
    CurrentUser: CurrentUser
    Identifier: Guid
    IsNew: bool
    NotifyCreated: Building -> unit
    NotifyEdited: Building -> unit
|}

let private getBuildingCmd buildingId =
    Cmd.OfAsync.either
        (Remoting.getRemotingApi().GetBuilding)
        buildingId
        View
        RemotingError

let init (props: DetailsProps): Model * Cmd<Msg> =
    let state, cmd =
        if props.IsNew then
            Creating (Building.Init props.Identifier, false), Cmd.none
        else
            Loading, getBuildingCmd props.Identifier
    { 
        CurrentUser = props.CurrentUser
        BuildingId = props.Identifier
        State = state
        NotifyCreated = props.NotifyCreated
        NotifyEdited = props.NotifyEdited
    }, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | View building ->
        match building with
        | Some building ->
            { model with State = Viewing building }, Cmd.none
        | None ->
            { model with State = BuildingNotFound }, Cmd.none
    | Save ->
        match model.State with
        | Editing (building, _) ->
            let cmd = 
                Cmd.OfAsync.either
                    (Remoting.getRemotingApi().UpdateBuilding)
                    (UpdateBuildingRequest.From building)
                    (fun result -> result |> Result.map (fun _ -> building) |> ProcessSaveResult)
                    RemotingError
            { model with State = Editing (building, true) }, cmd
        | Creating (building, _) ->
            let cmd =
                Cmd.OfAsync.either
                    (Remoting.getRemotingApi().CreateBuilding)
                    (CreateBuildingRequest.From building)
                    (fun result -> result |> Result.map (fun _ -> building) |> ProcessSaveResult)
                    RemotingError
            { model with State = Creating(building, true) }, cmd
        | _ ->
            //Do nothing, unexpected message O_o
            model, Cmd.none
    | RemotingError e ->
        { model with State = State.RemotingError e }, Cmd.none
    | ProcessSaveResult result ->
        match result with
        | Ok result ->
            match model.State with
            | Editing _ -> model.NotifyEdited result
            | Creating _ -> model.NotifyCreated result
            | _ -> ()
            { model with State = Viewing result }, Cmd.none
        | Error e ->
            //TODO!
            model, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    match model.State with
    | Loading ->  div [] [ str "Details worden geladen" ]
    | BuildingNotFound -> div [] [ str "Het door u gekozen gebouw werd niet gevonden in de databank..." ]
    | State.RemotingError e -> div [] [ str "Er is iets misgelopen bij het ophalen van de gegevens, gelieve de pagina te verversen" ]
    | Editing (detail, isSaving)
    | Creating (detail, isSaving) ->
        if isSaving 
        then
            div [] [ str "Het gebouw wordt bewaard" ]
        else
            div [] [ str "TODO: editeren en aanmaken van gebouwen" ]
    | Viewing detail ->
        div [] [
            fieldset [] [
                legend [] [ h2 [] [ str "Algemeen" ] ]
                div [] [
                    label [] [ str "Code" ]
                    p [] [ str detail.Code ]
                ]
                div [] [
                    label [] [ str "Naam" ]
                    p [] [ str detail.Name ]
                ]
                div [] [
                    label [] [ str "Adres" ]
                    p [] [ str detail.Address.Street ]
                ]
                div [] [
                    label [] [ str "Postcode" ]
                    p [] [ str detail.Address.ZipCode ]
                ]
                div [] [
                    label [] [ str "Woonplaats" ]
                    p [] [ str detail.Address.Town ]
                ]
                div [] [
                    match detail.OrganizationNumber with
                    | Some number ->
                        label [] [ str "Ondernemingsnummer" ]
                        p [] [ str number ]
                    | None ->
                        null
                ]
                div [] [
                    match detail.Remarks with
                    | Some remarks ->
                        label [] [ str "Opmerkingen" ]
                        p [] [ str remarks ]
                    | None ->
                        null
                ]
                div [] [
                    match detail.GeneralMeetingFrom, detail.GeneralMeetingUntil with
                    | Some from, Some until ->
                        label [] [ str "Periode algemene vergadering: " ]
                        p [] [ str (sprintf "Tussen %s en %s" (from.ToString("dd-MM-yyyy")) (until.ToString("dd-MM-yyyy"))) ]
                    | _ ->
                        null
                ]
                div [] [
                    label [] [ str "Actief" ]
                    p [] [ str (if detail.IsActive then "Ja" else "Nee") ]
                ]
            ]
            match detail.Concierge with
            | Some concierge ->
                fieldset [] [
                    legend [] [ h2 [] [ str "Concierge" ] ]
                    let person, isResident = 
                        match concierge with 
                        | Concierge.Resident resident -> resident.Person, true 
                        | Concierge.NonResident person -> person, false

                    div [] [
                        label [] [ str "Opmerking: je kan een inwoner linken aan een concierge, in dat geval worden gegevens automatisch geupdated wanneer de inwoner geupdated wordt" ]
                        label [] [ str "Inwoner van het gebouw?" ]
                        p [] [ str (if isResident then "Ja" else "Nee") ]
                    ]
                    div [] [
                        label [] [ str "Naam" ]
                        p [] [ str person.LastName ]
                    ]
                    div [] [
                        label [] [ str "Voornaam" ]
                        p [] [ str person.FirstName ]
                    ]
                    div [] [
                        label [] [ str "Contactgegevens" ]
                        p [] [ str "TODO..." ]
                    ]
                ]
            | None ->
                null
            match detail.Syndic with
            | Some syndic ->
                let person = 
                    match syndic with
                    | Syndic.Resident resident -> resident.Person
                    | Syndic.ProfessionalSyndic pro -> pro.Person
                    | Syndic.Other person -> person

                fieldset [] [
                    legend [] [ h2 [] [ str "Syndicus" ] ]
                    div [] [
                        label [] [ str "Net als bij de concierge, kan ook een inwoner gekoppeld worden aan de syndicus (of een pro syndicus -> nog in te stellen)" ]
                        label [] [ str "Type" ]
                        p [] [ 
                            str (match syndic with 
                                | Syndic.Resident _ -> "Inwoner"
                                | Syndic.ProfessionalSyndic _ -> "Professionele syndicus"
                                | Syndic.Other _ -> "Andere")  
                        ]
                    ]
                    div [] [
                        label [] [ str "Naam" ]
                        p [] [ str person.LastName ]
                    ]
                    div [] [
                        label [] [ str "Voornaam" ]
                        p [] [ str person.FirstName ]
                    ]
                    div [] [
                        label [] [ str "Contactgegevens" ]
                        p [] [ str "TODO..." ]
                    ]
                ]
            | None ->
                null
        ]


let render (props: DetailsProps) =
    React.elmishComponent ("Details", init props, update, view, string props.Identifier)

