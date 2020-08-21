﻿module Client.Buildings.BuildingEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Components
open Shared.Read
open Shared.Library

type Message =
    | NameChanged of string
    | CodeChanged of string
    | AddressChanged of Address
    | OrganizationNumberChanged of string
    | RemarksChanged of string
    | GeneralMeetingPeriodChanged of (DateTime * DateTime) option
    | YearOfConstructionChanged of string
    | YearOfDeliveryChanged of string

type State = {
    Building: Building
    GeneralMeetingPeriod: (DateTime * DateTime) option
    Errors: (string * string) list
}

let init (building: Building) =
    let today = DateTime.Today
    let dateTime (month, day) =
        new DateTime(today.Year, month, day)

    let period =
        building.GeneralMeetingPeriod 
        |> Option.map (fun p -> Some (dateTime(p.FromMonth, p.FromDay), dateTime(p.UntilDay, p.UntilMonth)))
        |> Option.defaultValue None

    { 
        Building = building
        GeneralMeetingPeriod = period
        Errors = []
    }, Cmd.none

let private formatOrganizationNumber (orgNr: string) =
    let digitString = orgNr |> String.filter Char.IsDigit
    match digitString.Length with
    | x when x > 7 -> sprintf "%s.%s.%s" digitString.[0..3] digitString.[4..6] digitString.[7..]
    | x when x > 4 -> sprintf "%s.%s" digitString.[0..3] digitString.[4..]
    | _ -> digitString

let update (message: Message) (state: State): State * Cmd<Message> =
    let changeBuilding f =
        { state with Building = f state.Building }

    let parseInt str =
        match str |> String.toOption with
        | Some s ->
            let isParsed, parsed = s |> String.filter Char.IsDigit |> Int32.TryParse
            if isParsed then Some parsed else None
        | None ->
            None


    match message with
    | NameChanged x ->
        changeBuilding (fun building -> { building with Name = x }), Cmd.none
    | CodeChanged x ->
        changeBuilding (fun b -> { b with Code = x }), Cmd.none
    | AddressChanged addr ->
        changeBuilding (fun b -> { b with Address = addr }), Cmd.none
    | OrganizationNumberChanged x ->
        let organizationNumber =
            x
            |> String.toOption
            |> Option.map formatOrganizationNumber

        changeBuilding (fun b -> { b with OrganizationNumber = organizationNumber }), Cmd.none
    | RemarksChanged x ->
        changeBuilding (fun b -> { b with Remarks = x |> String.toOption }), Cmd.none
    | GeneralMeetingPeriodChanged periodOption ->
        { state with GeneralMeetingPeriod = periodOption }, Cmd.none
    | YearOfConstructionChanged s ->
        changeBuilding (fun b -> { b with YearOfConstruction = parseInt s }), Cmd.none
    | YearOfDeliveryChanged s ->
        changeBuilding (fun b -> { b with YearOfDelivery = parseInt s }), Cmd.none

let inColomn x = div [ Class Bootstrap.col ] [ x ]

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
                    Helpers.valueOrDefault state.Building.Name
                    OnChange (fun e -> NameChanged e.Value |> dispatch)
                ] 
                FormError (errorFor (nameof state.Building.Name))
            ]
            |> inColomn
            formGroup [ 
                Label "Code"
                Input [ 
                    Type "text"
                    MaxLength 16.0
                    Helpers.valueOrDefault state.Building.Code
                    OnChange (fun e -> CodeChanged e.Value |> dispatch)
                ] 
                FormError (errorFor (nameof state.Building.Code))
            ]
            |> inColomn
        ]
        AddressEditComponent.render 
            ""
            state.Building.Address 
            (AddressChanged >> dispatch)
            (nameof state.Building.Address)
            state.Errors

        div [ Class Bootstrap.row ] [
            div [] [
                formGroup [
                    Label "Ondernemingsnr."
                    Input [ 
                        Type "text"
                        Pattern "[0-9]{4}\.[0-9]{3}\.[0-9]{3}"
                        MaxLength 12.0
                        Placeholder "xxxx.xxx.xxx"
                        Helpers.valueOrDefault state.Building.OrganizationNumber
                        OnChange (fun e -> OrganizationNumberChanged e.Value |> dispatch)
                    ] 
                    FormError (errorFor (nameof state.Building.OrganizationNumber))
                ]
                formGroup [
                    Label "Periode algemene vergadering"
                    Date [
                        match state.GeneralMeetingPeriod with
                        | Some (start, until) -> 
                            yield Flatpickr.Values [ start; until ]
                        | _ -> 
                            ()
                        yield 
                            Flatpickr.SelectionMode Flatpickr.Mode.Range
                        yield
                            Flatpickr.OnManyChanged (fun (dates: list<DateTime>) ->
                                 match dates with
                                 | [ fromDate; toDate ] -> GeneralMeetingPeriodChanged (Some (fromDate, toDate)) |> dispatch
                                 | _ -> GeneralMeetingPeriodChanged None |> dispatch)
                    ]
                ]
            ]
            |> inColomn
            formGroup [ 
                Label "Opmerkingen"
                TextArea [
                    Rows 4
                    Helpers.valueOrDefault state.Building.Remarks
                    OnChange (fun e -> RemarksChanged e.Value |> dispatch)
                ] 
            ]
            |> inColomn
        ]
        div [ Class Bootstrap.row ] [
            formGroup [
                Label "Bouwjaar"
                Input [
                    Type "number"
                    Helpers.valueOrDefault state.Building.YearOfConstruction
                    OnChange (fun e -> YearOfConstructionChanged e.Value |> dispatch)
                ]
                FormError (errorFor (nameof state.Building.YearOfConstruction))
            ]
            |> inColomn
            formGroup [
                Label "Opleveringsjaar"
                Input [
                    Type "number"
                    Helpers.valueOrDefault state.Building.YearOfDelivery
                    OnChange (fun e -> YearOfDeliveryChanged e.Value |> dispatch)
                ]
                FormError (errorFor (nameof state.Building.YearOfDelivery))
            ]
            |> inColomn
        ]
    ]