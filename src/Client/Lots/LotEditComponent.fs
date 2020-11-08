module Client.Lots.LotEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Shared.Read
open Shared.Write
open Shared.Library
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Library

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
    | StartDateChanged of int * DateTime
    | EndDateChanged of int * DateTime option
    | NoOp

type State = {
    Lot: Lot
    ShowingLotOwnerModal: bool
    Errors: (string * string) list
}

let init (lot: Lot) =
    {
        Lot = { lot with Owners = lot.Owners |> List.sortBy (fun owner -> owner.EndDate, owner.StartDate) }
        ShowingLotOwnerModal = false
        Errors = []
    }, Cmd.none

let update (message: Message) (state: State): State * Cmd<Message> =
    let changeLot f =
        { state with Lot = f state.Lot }

    let parseInt str =
        match str |> String.toOption with
        | Some s ->
            let isParsed, parsed = s |> String.filter Char.IsDigit |> Int32.TryParse
            if isParsed then Some parsed else None
        | None ->
            None

    let ensureLegalRepresentative (lotOwners: LotOwner list) =
        match lotOwners with
        | [] -> []
        | xs ->
            if not (xs |> List.exists (fun o -> o.LotOwnerRole = LegalRepresentative)) then
                { xs.[0] with LotOwnerRole = LegalRepresentative }::xs.[1..]
            else
                xs

    let recalculateValidationErrors (state: State) =
        match state.Errors with
        | [] -> state
        | _errors ->
            match ValidatedLot.Validate state.Lot with
            | Ok _validated -> state
            | Error validationErrors -> { state with Errors = validationErrors }

    match message with
    | CodeChanged x ->
        changeLot (fun l -> { l with Code = x }), Cmd.none
    | LotTypeChanged x ->
        changeLot (fun l -> { l with LotType = x }), Cmd.none
    | DescriptionChanged x ->
        changeLot (fun l -> { l with Description = x |> String.toOption }), Cmd.none
    | FloorChanged x ->
        changeLot (fun l -> { l with Floor = parseInt x }), Cmd.none
    | ChangeLotOwners ->
        { state with ShowingLotOwnerModal = true }, Cmd.none
    | ChangeLotOwnersCanceled ->
        { state with ShowingLotOwnerModal = false }, Cmd.none
    | AddLotOwnerOfType lotOwnerType ->
        let newLotOwner: LotOwner = {
            LotId = state.Lot.LotId
            LotOwnerType = lotOwnerType
            LotOwnerRole = LotOwnerRole.Other
            StartDate = DateTime.Today
            EndDate = None
        }
        let newState = changeLot (fun l -> { l with Owners = (newLotOwner::l.Owners) |> ensureLegalRepresentative })
        { newState with ShowingLotOwnerModal = false }, Cmd.none
    | RemoveLotOwner toRemove ->
        state, 
            showConfirmationModal 
                {|
                    Title = "Eigenaar verwijderen"
                    Message = "Bent u er zeker van dat u de koppeling tussen de eigenaar en het kavel wilt verbreken?"
                    OnConfirmed = (fun () -> ConfirmRemoveLotOwner toRemove)
                    OnDismissed = (fun () -> NoOp)
                |}
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
        changeLot (fun l -> { l with Share = parseInt x }), Cmd.none
    | NoOp ->
        state, Cmd.none
    | StartDateChanged (index, newStartDate) ->
        let newLotOwners = state.Lot.Owners |> List.mapi (fun idx o -> if idx = index then { o with StartDate = newStartDate } else o)
        changeLot (fun l -> { l with Owners = newLotOwners }), Cmd.none
    | EndDateChanged (index, newEndDate) ->
        let newLotOwners = state.Lot.Owners |> List.mapi (fun idx o -> if idx = index then { o with EndDate = newEndDate |> Option.map (fun ed -> ed.ToUniversalTime()) } else o)
        changeLot (fun l -> { l with Owners = newLotOwners }), Cmd.none

    |> (fun (state, cmd) -> state |> recalculateValidationErrors, cmd)

let renderEditLotOwners (basePath: string) (errors: (string * string) list) (lotOwners: LotOwner list) (dispatch: Message -> unit) =
    let errorMessageFor index s = 
        let path = sprintf "%s.[%i].%s" basePath index s
        match errors |> List.tryPick (fun (p, error) -> if p = path then Some error else None) with
        | Some error -> div [ Class Bootstrap.invalidFeedback ] [ str error ]
        | None -> null

    let ownerTypes (lotOwner: LotOwner) =
        match lotOwner.LotOwnerType with
        | LotOwnerType.Owner _ -> str "Persoon"
        | LotOwnerType.Organization o -> str ("Leverancier: " + (o.OrganizationTypeNames |> String.joinWith ", "))

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

    let startDateEditorFor (index: int) (lotOwner: LotOwner) =
        Flatpickr.flatpickr  [
            Flatpickr.OnChange (fun e -> StartDateChanged (index, e) |> dispatch)
            Flatpickr.Value lotOwner.StartDate
            Flatpickr.SelectionMode Flatpickr.Mode.Single
            Flatpickr.EnableTimePicker false
        ]

    let endDateEditorFor (index: int) (owner: LotOwner) =
        form [ Id (sprintf "%A" owner.LotOwnerType) ] [
            div [ Class Bootstrap.inputGroup ] [
                Flatpickr.flatpickr [
                    yield Flatpickr.OnChange (fun e -> EndDateChanged (index, Some e) |> dispatch)
                    match owner.EndDate with
                    | Some endDate -> yield Flatpickr.Value endDate
                    | None -> yield Flatpickr.custom "key" owner.EndDate false
                    yield Flatpickr.SelectionMode Flatpickr.Mode.Single
                    yield Flatpickr.EnableTimePicker false
                ]
                div [ Class Bootstrap.inputGroupAppend ] [
                    button [
                        Type "reset"
                        classes [ Bootstrap.btn; Bootstrap.btnDanger; Bootstrap.btnSm ] 
                        OnClick (fun _ -> EndDateChanged (index, None) |> dispatch)
                    ] [
                        str "×"
                    ]
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
                    h3 [] [ str "Eigenaar(s)" ]
                    p [] [ str "Bij een overdracht moet de einddatum van de vorige eigenaar(s) gezet worden op de dag vóór het verlijden van de akte en de begindatum van de nieuwe eigenaar(s) op de dag van het verlijden van de akte." ]
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
                            yield! owners |> List.mapi (fun index owner ->
                                tr [] [
                                    td [] [ ownerTypes owner ]
                                    td [] [ ownerName owner ]
                                    td [] [ isResident owner ]
                                    td [] [ 
                                        startDateEditorFor index owner
                                        errorMessageFor index (nameof owner.StartDate)
                                    ]
                                    td [] [ endDateEditorFor index owner ]
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
            h3 [] [ str "Algemeen" ]
        ]
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
                FormError (errorFor (nameof state.Lot.Description))
            ]
            |> inColomn Bootstrap.col
        ]

        renderEditLotOwners (nameof state.Lot.Owners) state.Errors state.Lot.Owners dispatch
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