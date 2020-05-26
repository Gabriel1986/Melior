module Client.Buildings.BuildingEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Components
open Shared.Domain
open Shared.Library

type Message =
    | BuildingNameChanged of string
    | BuildingCodeChanged of string
    | BuildingAddressChanged of Address
    | BuildingOrganizationNumberChanged of string * string * string
    | BuildingRemarksChanged of string
    | GeneralMeetingPeriodChanged of (DateTime * DateTime) option
    | BuildingYearOfConstructionChanged of string
    | BuildingYearOfDeliveryChanged of string
    | ChangeSyndic
    | SyndicChanged of Syndic option
    | SyndicChangeCanceled
    | ChangeConcierge
    | ConciergeChanged of Concierge option
    | ConciergeChangeCanceled

type State = {
    Building: Building
    OrganizationNumber: string * string * string
    GeneralMeetingPeriod: (DateTime * DateTime) option
    ShowingSyndicModal: bool
    ShowingConciergeModal: bool
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
        OrganizationNumber = 
            building.OrganizationNumber 
            |> Option.map (fun o -> o.ToStrings()) 
            |> Option.defaultValue ("", "", "")
        GeneralMeetingPeriod = period
        ShowingConciergeModal = false
        ShowingSyndicModal = false
    }, Cmd.none

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
    | BuildingNameChanged x ->
        changeBuilding (fun building -> { building with Name = x }), Cmd.none
    | BuildingCodeChanged x ->
        changeBuilding (fun b -> { b with Code = x }), Cmd.none
    | BuildingAddressChanged addr ->
        changeBuilding (fun b -> { b with Address = addr }), Cmd.none
    | BuildingOrganizationNumberChanged (x, y, z) ->
        { state with OrganizationNumber = x, y, z }, Cmd.none
    | BuildingRemarksChanged x ->
        changeBuilding (fun b -> { b with Remarks = x |> String.toOption }), Cmd.none
    | GeneralMeetingPeriodChanged periodOption ->
        { state with GeneralMeetingPeriod = periodOption }, Cmd.none
    | BuildingYearOfConstructionChanged s ->
        changeBuilding (fun b -> { b with YearOfConstruction = parseInt s }), Cmd.none
    | BuildingYearOfDeliveryChanged s ->
        changeBuilding (fun b -> { b with YearOfDelivery = parseInt s }), Cmd.none
    | ChangeSyndic ->
        { state with ShowingSyndicModal = true }, Cmd.none
    | SyndicChangeCanceled ->
        { state with ShowingSyndicModal = false }, Cmd.none
    | SyndicChanged newSyndic ->
        let newState = changeBuilding (fun b -> { b with Syndic = newSyndic })
        { newState with ShowingSyndicModal = false }, Cmd.none
    | ChangeConcierge ->
        { state with ShowingConciergeModal = true }, Cmd.none
    | ConciergeChangeCanceled ->
        { state with ShowingConciergeModal = false }, Cmd.none
    | ConciergeChanged newConcierge ->
        let newState = changeBuilding (fun b -> { b with Concierge = newConcierge })
        { newState with ShowingConciergeModal = false }, Cmd.none


let renderEditConcierge concierge dispatch =
    let conciergeName =
        let person = 
            match concierge with
            | Some (Concierge.Owner o)    -> Some o.Person
            | Some (Concierge.NonOwner p) -> Some p
            | None                        -> None
        person |> Option.map (fun p -> (p.FirstName |> Option.defaultValue "") + " " + (p.LastName |> Option.defaultValue ""))

    match conciergeName with
    | Some conciergeName ->
        div [ Class Bootstrap.formGroup ] [
            label [] [ str "Concierge" ]
            div [ Class Bootstrap.formInline ] [
                div [ Class Bootstrap.formGroup ] [
                    label [ Style [ MarginRight "5px" ] ] [ str conciergeName ]
                    button [ 
                        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary; Bootstrap.btnSm ]
                        Style [ MarginRight "5px" ]
                        OnClick (fun _ -> ChangeConcierge |> dispatch) 
                    ] [ i [ classes [ FontAwesome.fa; FontAwesome.faEdit ] ] [] ]
                    button [
                        classes [ Bootstrap.btn; Bootstrap.btnOutlineDanger; Bootstrap.btnSm ]
                        OnClick (fun _ -> ConciergeChanged None |> dispatch)
                    ] [ i [ classes [ FontAwesome.fa; FontAwesome.faTrash ] ] [] ]
                ]
            ]
        ]
    | None ->
        div [ Class Bootstrap.formGroup ] [
            button [ 
                classes [ Bootstrap.btn; Bootstrap.btnSecondary ]
                OnClick (fun _ -> ChangeConcierge |> dispatch) 
            ] [ str "Concierge toevoegen" ]
        ]

let renderEditSyndic syndic dispatch =
    let syndicName =
        let person = 
            match syndic with
            | Some (Syndic.Owner o)              -> Some o.Person
            | Some (Syndic.ProfessionalSyndic p) -> Some p.Person
            | Some (Syndic.Other p)              -> Some p
            | None                               -> None
        person |> Option.map (fun p -> (p.FirstName |> Option.defaultValue "") + " " + (p.LastName |> Option.defaultValue ""))

    match syndicName with
    | Some syndicName ->
        div [ Class Bootstrap.formGroup ] [
            label [] [ str "Syndicus" ]
            div [ Class Bootstrap.formInline ] [
                div [ Class Bootstrap.formGroup ] [
                    label [ Style [ MarginRight "5px" ] ] [ str syndicName ]
                    a [ 
                        Class "pointer"
                        Style [ MarginRight "5px" ]
                        OnClick (fun _ -> ChangeSyndic |> dispatch) 
                    ] [ i [ classes [ FontAwesome.fa; FontAwesome.faEdit ] ] [] ]
                    a [
                        Class "pointer"
                        OnClick (fun _ -> SyndicChanged None |> dispatch)
                    ] [ i [ classes [ FontAwesome.fa; FontAwesome.faTrashAlt ] ] [] ]
                ]
            ]
        ]
    | None ->
        div [ Class Bootstrap.formGroup ] [
            button [
                classes [ Bootstrap.btn; Bootstrap.btnSecondary ]
                OnClick (fun _ -> ChangeSyndic |> dispatch) 
            ] [ str "Syndicus toevoegen" ]
        ]

let view (state: State) (dispatch: Message -> unit) =
    let syndicName =
        let person =
            function
            | Syndic.Owner o -> o.Person
            | Syndic.ProfessionalSyndic p -> p.Person
            | Syndic.Other p -> p
        person >> (fun p -> (p.FirstName |> Option.defaultValue "") + " " + (p.LastName |> Option.defaultValue ""))

    div [] [
        formGroup [ 
            Label "Naam"
            Input [ 
                Type "text"
                MaxLength 255.0
                Helpers.valueOrDefault state.Building.Name
                OnChange (fun e -> BuildingNameChanged e.Value |> dispatch)
            ] 
        ]
        formGroup [ 
            Label "Code"
            Input [ 
                Type "text"
                MaxLength 16.0
                Helpers.valueOrDefault state.Building.Code
                OnChange (fun e -> BuildingCodeChanged e.Value |> dispatch)
            ] 
        ]
        AddressEditComponent.render "" state.Building.Address (BuildingAddressChanged >> dispatch)
        OrganizationNumberEditComponent.render "Ondernemingsnr." state.OrganizationNumber (BuildingOrganizationNumberChanged >> dispatch)
        formGroup [ 
            Label "Opmerkingen"
            Input [ 
                Type "text"
                Helpers.valueOrDefault state.Building.Remarks
                OnChange (fun e -> BuildingRemarksChanged e.Value |> dispatch)
            ] 
        ]
        formGroup [
            Label "Algemene vergadering periode"
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
        renderEditConcierge state.Building.Concierge dispatch
        renderEditSyndic state.Building.Syndic dispatch
        formGroup [
            Label "Bouwjaar"
            Input [
                Type "number"
                Helpers.valueOrDefault state.Building.YearOfConstruction
                OnChange (fun e -> BuildingYearOfConstructionChanged e.Value |> dispatch)
            ]
        ]
        formGroup [
            Label "Opleveringsjaar"
            Input [
                Type "number"
                Helpers.valueOrDefault state.Building.YearOfDelivery
                OnChange (fun e -> BuildingYearOfDeliveryChanged e.Value |> dispatch)
            ]
        ]

        SyndicModal.render 
            {|
                IsOpen = state.ShowingSyndicModal
                BuildingId = state.Building.BuildingId
                Concierge = state.Building.Syndic
                OnConciergeChanged = (fun s -> SyndicChanged (Some s) |> dispatch)
                OnCanceled = (fun _ -> SyndicChangeCanceled |> dispatch) 
            |}

        ConciergeModal.render
            {|
                IsOpen = state.ShowingConciergeModal
                BuildingId = state.Building.BuildingId
                Concierge = state.Building.Concierge
                OnConciergeChanged = (fun c -> ConciergeChanged (Some c) |> dispatch)
                OnCanceled = (fun _ -> ConciergeChangeCanceled |> dispatch)
            |}
    ]