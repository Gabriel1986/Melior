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
open Client.Routing

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
    | ShareChanged of string
    | StartDateChanged of int * DateTimeOffset
    | EndDateChanged of int * DateTimeOffset option
    | OpenContactModal of int * LotOwnerContactModal.CreateOrUpdate
    | CloseContactModal
    | SaveContactModal of LotOwnerContact
    | RemoveContact of int * int
    | ConfirmRemoveContact of int * int
    | NoOp

type State = {
    Lot: Lot
    ShowingLotOwnerModal: bool
    ShowingLotOwnerContactModalOn: (int * LotOwnerContactModal.CreateOrUpdate) option
    Errors: (string * string) list
}

let init (lot: Lot) =
    {
        Lot = lot
        ShowingLotOwnerModal = false
        ShowingLotOwnerContactModalOn = None
        Errors = []
    }, Cmd.none

let update (message: Message) (state: State): State * Cmd<Message> =
    let changeLot f =
        { state with Lot = f state.Lot }

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
            LotOwnerId = Guid.NewGuid()
            BuildingId = state.Lot.BuildingId
            LotOwnerType = lotOwnerType
            StartDate = DateTimeOffset.Now
            EndDate = None
            Contacts = []
        }
        let newState = changeLot (fun l -> { l with Owners = newLotOwner::l.Owners })
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
        let newLotOwners = state.Lot.Owners |> List.filter ((<>) toRemove)
        changeLot (fun l -> { l with Owners = newLotOwners }), Cmd.none
    | ShareChanged x ->
        changeLot (fun l -> { l with Share = parseInt x }), Cmd.none
    | NoOp ->
        state, Cmd.none
    | StartDateChanged (index, newStartDate) ->
        let newLotOwners = state.Lot.Owners |> List.mapi (fun idx o -> if idx = index then { o with StartDate = newStartDate } else o)
        changeLot (fun l -> { l with Owners = newLotOwners }), Cmd.none
    | EndDateChanged (index, newEndDate) ->
        let newLotOwners = state.Lot.Owners |> List.mapi (fun idx o -> if idx = index then { o with EndDate = newEndDate } else o)
        changeLot (fun l -> { l with Owners = newLotOwners }), Cmd.none
    | OpenContactModal (index, createOrUpdate) ->
        { state with ShowingLotOwnerContactModalOn = Some (index, createOrUpdate) }, Cmd.none
    | CloseContactModal ->
        { state with ShowingLotOwnerContactModalOn = None }, Cmd.none
    | SaveContactModal contact ->
        let replaceContactIn (contacts: LotOwnerContact list) =
            contacts
            |> List.map (fun existing -> if existing.PersonId = contact.PersonId then contact else existing)

        let updatedOwners =
            match state.ShowingLotOwnerContactModalOn with
            | Some (index, LotOwnerContactModal.Create) ->
                state.Lot.Owners
                |> List.mapi (fun idx owner -> if idx = index then { owner with Contacts = owner.Contacts@[contact] } else owner)
            | Some (index, LotOwnerContactModal.Update _) ->
                state.Lot.Owners
                |> List.mapi (fun idx owner -> if idx = index then { owner with Contacts = replaceContactIn owner.Contacts } else owner)
            | None ->
                state.Lot.Owners
        { state with Lot = { state.Lot with Owners = updatedOwners }; ShowingLotOwnerContactModalOn = None }, Cmd.none
    | RemoveContact (index, otherIndex) ->
        state, 
            showConfirmationModal 
                {| 
                    Title = "Contact verwijderen"
                    Message = "Bent u er zeker van dat u de contactpersoon wilt verwijderen?"
                    OnConfirmed = (fun () -> ConfirmRemoveContact (index, otherIndex))
                    OnDismissed = (fun () -> NoOp) 
                |}
    | ConfirmRemoveContact (index, otherIndex) ->
        let removeContactFromLotOwner (lotOwner: LotOwner) =
            let updatedContacts =
                lotOwner.Contacts 
                |> List.indexed 
                |> List.choose (fun (otherIdx, contact) -> if otherIdx = otherIndex then None else Some contact)
            { lotOwner with Contacts = updatedContacts }
        let updatedOwners =
            state.Lot.Owners
            |> List.mapi (fun idx owner -> if idx = index then removeContactFromLotOwner owner else owner)
        { state with Lot = { state.Lot with Owners = updatedOwners } }, Cmd.none
    |> (fun (state, cmd) -> state |> recalculateValidationErrors, cmd)

let renderEditLotOwners (state: State) (dispatch: Message -> unit) =
    let errorMessageFor index s = 
        let path = sprintf "%s.[%i].%s" (nameof (state.Lot.Owners)) index s
        match state.Errors |> List.tryPick (fun (p, error) -> if p = path then Some error else None) with
        | Some error -> div [ Class Bootstrap.invalidFeedback ] [ str error ]
        | None -> null

    let ownerTypes (lotOwner: LotOwner) =
        match lotOwner.LotOwnerType with
        | LotOwnerType.Owner _ -> str "Persoon"
        | LotOwnerType.Organization o -> str ("Leverancier: " + (o.OrganizationTypeNames |> String.joinWith ", "))

    let ownerName (lotOwner: LotOwner) =
        match lotOwner.LotOwnerType with
        | LotOwnerType.Owner o        -> (o.FullName ())
        | LotOwnerType.Organization o -> o.Name

    let isResident (lotOwner: LotOwner) =
        match lotOwner.LotOwnerType with
        | LotOwnerType.Owner o        -> str (if o.IsResident then "Ja" else "Nee")
        | LotOwnerType.Organization _ -> str ""

    let startDateEditorFor (index: int) (lotOwner: LotOwner) =
        let errorMessage = errorMessageFor index (nameof lotOwner.StartDate)
        div [ Style [ MaxWidth "160px" ] ] [
            Flatpickr.flatpickr  [
                Flatpickr.OnChange (fun e -> StartDateChanged (index, new DateTimeOffset(e)) |> dispatch)
                Flatpickr.Value lotOwner.StartDate.DateTime
                Flatpickr.SelectionMode Flatpickr.Mode.Single
                Flatpickr.EnableTimePicker false
                Flatpickr.Locale Flatpickr.Locales.dutch
                Flatpickr.DateFormat "d/m/Y"
                Flatpickr.ClassName (if errorMessage <> null then (sprintf "%s %s" Bootstrap.isInvalid Bootstrap.formControl) else "")
            ]
            errorMessage
        ]

    let endDateEditorFor (index: int) (owner: LotOwner) =
        form [ Id (sprintf "%A" owner.LotOwnerId) ] [
            div [ Class Bootstrap.inputGroup ] [
                Flatpickr.flatpickr [
                    Flatpickr.OnChange (fun e -> EndDateChanged (index, Some (new DateTimeOffset(e))) |> dispatch)
                    match owner.EndDate with
                    | Some endDate -> Flatpickr.Value endDate.LocalDateTime
                    | None -> Flatpickr.custom "key" owner.EndDate false
                    Flatpickr.SelectionMode Flatpickr.Mode.Single
                    Flatpickr.EnableTimePicker false
                    Flatpickr.Locale Flatpickr.Locales.dutch
                    Flatpickr.DateFormat "d/m/Y"
                ]
                div [ Class Bootstrap.inputGroupAppend ] [
                    button [
                        Type "reset"
                        classes [ Bootstrap.btn; Bootstrap.btnOutlineDanger; Bootstrap.btnSm ] 
                        OnClick (fun _ -> EndDateChanged (index, None) |> dispatch)
                    ] [
                        str "×"
                    ]
                ]
            ]
        ]

    div [ Class Bootstrap.col12 ] [
        yield
            match state.Lot.Owners with
            | [] ->
                null
            | owners ->
                div [ Class Bootstrap.formGroup ] [
                    h3 [] [ str "Eigenaar(s)" ]
                    p [ Class Bootstrap.textInfo ] [
                        i [ classes [ FontAwesome.fa; FontAwesome.faInfoCircle ] ] []
                        str " "
                        str "Bij een overdracht moet de einddatum van de vorige eigenaar(s) gezet worden op de dag vóór het verlijden van de akte en de begindatum van de nieuwe eigenaar(s) op de dag van het verlijden van de akte." 
                    ]
                    table [ classes [ Bootstrap.table; Bootstrap.tableStriped ] ] [
                        thead [] [
                            tr [] [
                                th [] [ str "Naam" ]
                                th [] [ str "Begindatum" ]
                                th [] [ str "Einddatum" ]
                                th [] []
                            ]
                        ]
                        tbody [] [
                            yield! owners |> List.mapi (fun index owner ->
                                [
                                    tr [ Key (string owner.LotOwnerId) ] [
                                        td [] [ str (ownerName owner) ]
                                        td [] [ startDateEditorFor index owner ]
                                        td [] [ endDateEditorFor index owner ]
                                        td [ Class Bootstrap.textRight ] [
                                            button [
                                                classes [ Bootstrap.btn; Bootstrap.btnSm; Bootstrap.btnOutlineDanger ]
                                                OnClick (fun _ -> RemoveLotOwner owner |> dispatch)
                                            ] [
                                                i [ classes [ FontAwesome.fa; FontAwesome.faTrashAlt ] ] []
                                                str " "
                                                str "Verwijderen"
                                            ]
                                        ]
                                    ]
                                    tr [] [
                                        td [ ColSpan 4 ] [
                                            if not owner.Contacts.IsEmpty then h6 [ Class Bootstrap.ml4 ] [ str "Contactpersonen" ]
                                            ul [ classes [ Bootstrap.listGroup; Bootstrap.ml4 ] ] [
                                                yield! owner.Contacts |> List.mapi (fun otherIndex contact ->
                                                    li [ Key (string contact.PersonId); Class Bootstrap.listGroupItem ] [
                                                        match contact with
                                                        | LotOwnerContact.Owner o ->
                                                            [
                                                                str (o.FullName ())
                                                                div [ Class Bootstrap.floatRight ] [
                                                                    button [
                                                                        classes [ Bootstrap.btn; Bootstrap.btnSm; Bootstrap.btnOutlineDanger ]
                                                                        OnClick (fun _ -> RemoveContact (index, otherIndex) |> dispatch)
                                                                    ] [
                                                                        i [ classes [ FontAwesome.fa; FontAwesome.faTrashAlt ] ] []
                                                                        str " "
                                                                        str "Verwijderen"
                                                                    ]
                                                                ]
                                                            ]
                                                            |> fragment []
                                                        | LotOwnerContact.NonOwner person ->
                                                            [
                                                                str (person.FullName())
                                                                div [ Class Bootstrap.floatRight ] [
                                                                    button [
                                                                        classes [ Bootstrap.btn; Bootstrap.btnSm; Bootstrap.btnOutlinePrimary ]
                                                                        OnClick (fun _ -> OpenContactModal (index, LotOwnerContactModal.Update (LotOwnerContact.NonOwner person)) |> dispatch)
                                                                    ] [
                                                                        i [ classes [ FontAwesome.fa; FontAwesome.faEdit; Bootstrap.textPrimary ] ] []
                                                                        str " "
                                                                        str "Aanpassen"
                                                                    ]
                                                                    str " "
                                                                    button [
                                                                        classes [ Bootstrap.btn; Bootstrap.btnSm; Bootstrap.btnOutlineDanger ]
                                                                        OnClick (fun _ -> RemoveContact (index, otherIndex) |> dispatch)
                                                                    ] [
                                                                        i [ classes [ FontAwesome.fa; FontAwesome.faTrashAlt ] ] []
                                                                        str " "
                                                                        str "Verwijderen"
                                                                    ]
                                                                ]
                                                            ]
                                                            |> fragment []
                                                    ]
                                                )
                                            ]
                                            button [
                                                classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary; Bootstrap.ml4; Bootstrap.mt2 ]
                                                OnClick (fun _ -> OpenContactModal (index, LotOwnerContactModal.Create) |> dispatch)
                                            ] [
                                                i [ classes [ FontAwesome.fa; FontAwesome.faUserPlus ] ] []
                                                str " "
                                                str (sprintf "Contactpersoon toevoegen voor %s" (ownerName owner))
                                            ]
                                        ]
                                    ]
                                ]
                                |> fragment []
                            )
                        ]
                    ]
                ]
        yield div [ Class Bootstrap.formGroup ] [
            button [ 
                classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary ]
                OnClick (fun _ -> ChangeLotOwners |> dispatch) 
            ] [
                i [ classes [ FontAwesome.fa; FontAwesome.faPlus ] ] []
                str " "
                str "Nieuwe eigenaar toekennen" 
            ]
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
            div [ Class Bootstrap.col12 ] [
                h3 [] [ str "Algemeen" ]
            ]
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
                FieldError (errorFor (nameof state.Lot.Code))
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
                            FieldError (errorFor (nameof state.Lot.Floor))
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
                        FieldError (errorFor (nameof state.Lot.Share))
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
                FieldError (errorFor (nameof state.Lot.Description))
            ]
            |> inColomn Bootstrap.col
        ]

        renderEditLotOwners state dispatch
        |> inColomn Bootstrap.row

        LotOwnerTypeModal.render 
            {|
                IsOpen = state.ShowingLotOwnerModal
                BuildingId = state.Lot.BuildingId
                LotOwnerTypes = state.Lot.Owners |> List.map (fun ownerType -> ownerType.LotOwnerType)
                OnSelected = (fun owners -> AddLotOwnerOfType owners |> dispatch)
                OnCanceled = (fun _ -> ChangeLotOwnersCanceled |> dispatch) 
            |}

        match state.ShowingLotOwnerContactModalOn with
        | Some (_, createOrUpdate) ->
            LotOwnerContactModal.render
                {|
                    IsOpen = state.ShowingLotOwnerContactModalOn.IsSome
                    BuildingId = state.Lot.BuildingId
                    CreateOrUpdate = createOrUpdate
                    OnContactChanged = fun contact -> SaveContactModal contact |> dispatch
                    OnCanceled = fun () -> dispatch CloseContactModal
                |}
        | None -> 
            null
    ]