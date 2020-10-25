module Client.Lots.LotEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Library
open Shared.Read
open Shared.Library

type Message =
    | CodeChanged of string
    | LotTypeChanged of LotType
    | DescriptionChanged of string
    | FloorChanged of string
    | ChangeLotOwners
    | AddLotOwnerOfType of LotOwnerType
    | ChangeLotOwnersCanceled
    | RemoveLotOwner of LotOwner
    | ConfirmRemoveLotOwner of LotOwner
    | SelectLegalRepresentative of LotOwner
    | ShareChanged of string
    | StartDateChanged of LotOwner * DateTime
    | EndDateChanged of LotOwner * DateTime option
    | NoOp

type State = {
    Lot: Lot
    ShowingLotOwnerModal: bool
    Errors: (string * string) list
}

let init (lot: Lot) =
    {
        Lot = lot
        ShowingLotOwnerModal = false
        Errors = []
    }, Cmd.none

let update (message: Message) (state: State): State * Cmd<Message> =
    let changeLot f =
        { state with Lot = f state.Lot }

    let removeError errorName state =
        { state with Errors = (state.Errors |> List.filter (fun (path, e) -> path <> errorName)) }

    let parseInt str =
        match str |> String.toOption with
        | Some s ->
            let isParsed, parsed = s |> String.filter Char.IsDigit |> Int32.TryParse
            if isParsed then Some parsed else None
        | None ->
            None

    let ensureLegalRepresentative lotOwners =
        match lotOwners with
        | [] -> []
        | xs ->
            if not (xs |> List.exists (fun o -> o.LotOwnerRole = LegalRepresentative)) then
                { xs.[0] with LotOwnerRole = LegalRepresentative }::xs.[1..]
            else
                xs

    match message with
    | CodeChanged x ->
        changeLot (fun l -> { l with Code = x })
        |> removeError (nameof state.Lot.Code), Cmd.none
    | LotTypeChanged x ->
        changeLot (fun l -> { l with LotType = x })
        |> removeError (nameof state.Lot.LotType), Cmd.none
    | DescriptionChanged x ->
        changeLot (fun l -> { l with Description = x |> String.toOption })
        |> removeError (nameof state.Lot.Description), Cmd.none
    | FloorChanged x ->
        changeLot (fun l -> { l with Floor = parseInt x })
        |> removeError (nameof state.Lot.Floor), Cmd.none
    | ChangeLotOwners ->
        { state with ShowingLotOwnerModal = true }, Cmd.none
    | ChangeLotOwnersCanceled ->
        { state with ShowingLotOwnerModal = false }, Cmd.none
    | AddLotOwnerOfType lotOwnerType ->
        let newLotOwner: LotOwner = {
            LotId = state.Lot.LotId
            LotOwnerType = lotOwnerType
            LotOwnerRole = LotOwnerRole.LegalRepresentative
            StartDate = DateTime.Today
            EndDate = None
        }
        let newState = changeLot (fun l -> { l with Owners = newLotOwner::l.Owners })
        { newState with ShowingLotOwnerModal = false }, Cmd.none
    | RemoveLotOwner toRemove ->
        state, 
            showConfirmationModal ( 
                "Eigenaar verwijderen", 
                "Bent u er zeker van dat u de koppeling tussen de eigenaar en het kavel wilt verbreken?",
                (fun () -> ConfirmRemoveLotOwner toRemove),
                (fun () -> NoOp)
            )
    | ConfirmRemoveLotOwner toRemove ->
        let newLotOwners = state.Lot.Owners |> List.filter (fun o -> o <> toRemove) |> ensureLegalRepresentative
        changeLot (fun l -> { l with Owners = newLotOwners }), Cmd.none
    | SelectLegalRepresentative lotOwner ->
        let newLotOwners = 
            state.Lot.Owners 
            |> List.map (fun o -> 
                if o = lotOwner 
                then { o with LotOwnerRole = LotOwnerRole.LegalRepresentative }
                else { o with LotOwnerRole = LotOwnerRole.Other })

        changeLot (fun l -> { l with Owners = newLotOwners }), Cmd.none
    | ShareChanged x ->
        changeLot (fun l -> { l with Share = parseInt x })
        |> removeError (nameof state.Lot.Share), Cmd.none
    | NoOp ->
        state, Cmd.none
    | StartDateChanged (lotOwner, newStartDate) ->
        let newLotOwners = state.Lot.Owners |> List.map (fun o -> if o = lotOwner then { o with StartDate = newStartDate } else o)
        changeLot (fun l -> { l with Owners = newLotOwners }), Cmd.none
    | EndDateChanged (lotOwner, newEndDate) ->
        let newLotOwners = state.Lot.Owners |> List.map (fun o -> if o = lotOwner then { o with EndDate = newEndDate } else o)
        changeLot (fun l -> { l with Owners = newLotOwners }), Cmd.none

let renderEditLotOwners lotOwners dispatch =
    let ownerTypes (lotOwner: LotOwner) =
        match lotOwner.LotOwnerType with
        | LotOwnerType.Owner _ -> str "Persoon"
        | LotOwnerType.Organization o -> str ("Leverancier: " + (o.OrganizationTypeNames |> String.JoinWith ", "))

    let ownerName (lotOwner: LotOwner) =
        match lotOwner.LotOwnerType with
        | LotOwnerType.Owner o        -> str (o.FullName ())
        | LotOwnerType.Organization o -> str o.Name

    let isResident (lotOwner: LotOwner) =
        match lotOwner.LotOwnerType with
        | LotOwnerType.Owner o        -> str (if o.IsResident then "Ja" else "Nee")
        | LotOwnerType.Organization _ -> str ""

    let isLegalRepresentative (lotOwner: LotOwner) =
        if lotOwner.LotOwnerRole = LegalRepresentative then
            button [ classes [ Bootstrap.btn; Bootstrap.btnPrimary; Bootstrap.btnSm ] ] [ str "Ja" ]
        else
            button [ 
                classes [ Bootstrap.btn; Bootstrap.btnLight; Bootstrap.btnSm ]
                OnClick (fun _ -> SelectLegalRepresentative lotOwner |> dispatch)
                Disabled (lotOwner.EndDate.IsSome && lotOwner.EndDate.Value < DateTime.Today)
            ] [
                str "Nee"
            ]

    let startDateEditorFor (lotOwner: LotOwner) =
        formGroup [
            Date [
                Flatpickr.OnChange (fun e -> StartDateChanged (lotOwner, e) |> dispatch)
                Flatpickr.Value lotOwner.StartDate
                Flatpickr.SelectionMode Flatpickr.Mode.Single
                Flatpickr.EnableTimePicker false
            ]
        ]

    let endDateEditorFor (owner: LotOwner) =
        formGroup [
            Date [
                yield Flatpickr.OnChange (fun e -> EndDateChanged (owner, Some e) |> dispatch)
                match owner.EndDate with
                | Some endDate -> yield Flatpickr.Value endDate
                | None -> ()
                yield Flatpickr.SelectionMode Flatpickr.Mode.Single
                yield Flatpickr.EnableTimePicker false
            ]
            OtherChildren [
                button [ 
                    classes [ Bootstrap.btn; Bootstrap.btnOutlineDanger ] 
                    OnClick (fun _ -> EndDateChanged (owner, None) |> dispatch)
                ] [
                    i [ classes [ FontAwesome.fa; FontAwesome.faTrashAlt ] ] []
                ]
            ]
        ]

    div [ Class Bootstrap.col12 ] [
        yield
            match lotOwners with
            | [] ->
                null
            | owners ->
                div [ Class Bootstrap.formGroup ] [
                    label [] [ str "Eigenaar(s)" ]
                    table [ classes [ Bootstrap.table; Bootstrap.tableStriped; Bootstrap.tableHover ] ] [
                        thead [] [
                            tr [] [
                                th [] [ str "Type(s)" ]
                                th [] [ str "Naam" ]
                                th [] [ str "Bewoner" ]
                                th [] [ str "Begindatum" ]
                                th [] [ str "Einddatum" ]
                                th [] [ str "Stemhouder" ]
                                th [] []
                            ]
                        ]
                        tbody [] [
                            yield! owners |> List.map (fun owner -> 
                                tr [] [
                                    td [] [ ownerTypes owner ]
                                    td [] [ ownerName owner ]
                                    td [] [ isResident owner ]
                                    td [] [ startDateEditorFor owner ]
                                    td [] [ endDateEditorFor owner ]
                                    td [] [ isLegalRepresentative owner ]
                                    td [] [ 
                                        a [
                                            Class "pointer"
                                            OnClick (fun _ -> RemoveLotOwner owner |> dispatch)
                                        ] [
                                            i [ classes [ FontAwesome.fa; FontAwesome.faTrashAlt ] ] []
                                        ]
                                    ]
                                ])
                        ]
                    ]
                ]
        yield div [ Class Bootstrap.formGroup ] [
            button [ 
                classes [ Bootstrap.btn; Bootstrap.btnSecondary ]
                OnClick (fun _ -> ChangeLotOwners |> dispatch) 
            ] [ str "Eigenaar(s) aanduiden" ]
        ]
    ]


let inColomn columnClass x = div [ Class columnClass ] [ x ]

let view (state: State) (dispatch: Message -> unit) =
    let errorFor (path: string) =
        state.Errors |> List.tryPick (fun (p, error) -> if p = path then Some error else None)

    let toOption (lotType: LotType): FormSelectOption = {
        Key = string lotType
        Label = string lotType
        IsSelected = state.Lot.LotType = lotType
    }

    let lotTypeSelection: FormSelect = {
        Identifier = string state.Lot.LotId
        OnChanged = (fun e ->
            let lotType = LotType.OfString e     
            LotTypeChanged lotType |> dispatch
        )
        Options = 
            [ Appartment; Studio; ParkingSpace; CommercialProperty; Garage; Storage; LotType.Other ]
            |> List.map toOption
    }

    div [] [
        div [ Class Bootstrap.row ] [
            formGroup [ 
                Label "Code"
                Input [ 
                    Type "text"
                    MaxLength 16.0
                    Helpers.valueOrDefault state.Lot.Code
                    OnChange (fun e -> CodeChanged e.Value |> dispatch)
                ]
                FormError (errorFor (nameof state.Lot.Code))
            ]
            |> inColomn Bootstrap.colMd4

            formGroup [ 
                Label "Type"
                Select lotTypeSelection
            ]
            |> inColomn Bootstrap.col
        ]
        div [ Class Bootstrap.row ] [
            div [] [
                div [ Class Bootstrap.row ] [
                    div [ Class Bootstrap.col; Style [ PaddingLeft "0" ] ] [
                        formGroup [
                            Label "Verdieping"
                            Input [
                                Type "number"
                                Min -20
                                Max 50
                                Helpers.valueOrDefault state.Lot.Floor
                                OnChange (fun e -> FloorChanged e.Value |> dispatch)
                            ]
                            FormError (errorFor (nameof state.Lot.Floor))
                        ]
                    ]
                ]
                div [] [
                    formGroup [
                        Label "Quotiteit"
                        Input [
                            Type "number"
                            Min 0
                            Helpers.valueOrDefault state.Lot.Share
                            OnChange (fun e -> ShareChanged e.Value |> dispatch)
                        ]
                        FormError (errorFor (nameof state.Lot.Share))
                    ]
                ]
            ]
            |> inColomn Bootstrap.colMd4
            formGroup [ 
                Label "Omschrijving"
                TextArea [
                    Rows 4
                    Helpers.valueOrDefault state.Lot.Description
                    OnChange (fun e -> DescriptionChanged e.Value |> dispatch)
                ] 
            ]
            |> inColomn Bootstrap.col
        ]

        renderEditLotOwners state.Lot.Owners dispatch
        |> inColomn Bootstrap.row

        LotOwnerTypeModal.render 
            {|
                IsOpen = state.ShowingLotOwnerModal
                BuildingId = state.Lot.BuildingId
                LotOwnerTypes = state.Lot.Owners |> List.map (fun ownerType -> ownerType.LotOwnerType)
                OnSelected = (fun owners -> AddLotOwnerOfType owners |> dispatch)
                OnCanceled = (fun _ -> ChangeLotOwnersCanceled |> dispatch) 
            |}
    ]