module Client.Lots.LotEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Shared.Read
open Shared.Library

type Message =
    | CodeChanged of string
    | LotTypeChanged of LotType
    | DescriptionChanged of string
    | FloorChanged of string
    | SurfaceChanged of string
    | ChangeLotOwners
    | LotOwnersChanged of LotOwner list
    | ChangeLotOwnersCanceled
    | RemoveLotOwner of LotOwner
    | SelectLegalRepresentative of LotOwner
    | ShareChanged of string

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
            if not (xs |> List.exists (fun (_, role) -> role = LegalRepresentative)) then
                (fst xs.[0], LegalRepresentative)::xs.[1..]
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
    | SurfaceChanged x ->
        changeLot (fun l -> { l with Surface = parseInt x })
        |> removeError (nameof state.Lot.Surface), Cmd.none
    | ChangeLotOwners ->
        { state with ShowingLotOwnerModal = true }, Cmd.none
    | ChangeLotOwnersCanceled ->
        { state with ShowingLotOwnerModal = false }, Cmd.none
    | LotOwnersChanged lotOwners ->
        let newLotOwners =
            match lotOwners with
            | [] -> []
            | lotOwners ->
                let mapLotOwnerToLotOwnerAndRole lotOwner = 
                    state.Lot.Owners 
                    |> List.tryPick (fun ownerWithRole -> 
                        if fst ownerWithRole = lotOwner 
                        then Some ownerWithRole 
                        else None)
                    |> Option.defaultWith (fun () -> lotOwner, LotOwnerRole.Other)

                lotOwners 
                |> List.map mapLotOwnerToLotOwnerAndRole
                |> ensureLegalRepresentative 
                
        let newState = changeLot (fun l -> { l with Owners = newLotOwners })
        { newState with ShowingLotOwnerModal = false }, Cmd.none
    | RemoveLotOwner lotOwner ->
        let newLotOwners = 
            state.Lot.Owners 
            |> List.filter (fun (o, _) -> o <> lotOwner)
            |> ensureLegalRepresentative

        changeLot (fun l -> { l with Owners = newLotOwners }), Cmd.none
    | SelectLegalRepresentative lotOwner ->
        let newLotOwners = 
            state.Lot.Owners 
            |> List.map (fun (o, _) -> 
                if o = lotOwner 
                then (o, LotOwnerRole.LegalRepresentative) 
                else (o, LotOwnerRole.Other))

        changeLot (fun l -> { l with Owners = newLotOwners }), Cmd.none
    | ShareChanged x ->
        changeLot (fun l -> { l with Share = parseInt x })
        |> removeError (nameof state.Lot.Share), Cmd.none

let renderEditLotOwners lotOwners dispatch =
    let ownerTypes lotOwner =
        match lotOwner with
        | LotOwner.Owner _ -> str "Persoon"
        | LotOwner.Organization o -> str ("Leverancier: " + (o.OrganizationTypeNames |> String.JoinWith ", "))

    let ownerName lotOwner =
        match lotOwner with
        | LotOwner.Owner o        -> str (o.FullName ())
        | LotOwner.Organization o -> str o.Name

    let isResident lotOwner =
        match lotOwner with
        | LotOwner.Owner o        -> str (if o.IsResident then "Ja" else "Nee")
        | LotOwner.Organization _ -> str ""

    let isLegalRepresentative (lotOwner: LotOwner, lotOwnerRole: LotOwnerRole) =
        if lotOwnerRole = LegalRepresentative then
            button [ classes [ Bootstrap.btn; Bootstrap.btnPrimary; Bootstrap.btnSm ] ] [ str "Ja" ]
        else
            button [ classes [ Bootstrap.btn; Bootstrap.btnLight; Bootstrap.btnSm ]; OnClick (fun _ -> SelectLegalRepresentative lotOwner |> dispatch) ] [ str "Nee" ]

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
                                th [] [ str "Inwoner" ]
                                th [] [ str "Stemhouder" ]
                                th [] []
                            ]
                        ]
                        tbody [] [
                            yield! owners |> List.map (fun owner ->
                                tr [] [
                                    td [] [ ownerTypes (fst owner) ]
                                    td [] [ ownerName (fst owner) ]
                                    td [] [ isResident (fst owner) ]
                                    td [] [ isLegalRepresentative owner ]
                                    td [] [ 
                                        a [
                                            Class "pointer"
                                            OnClick (fun _ -> RemoveLotOwner (fst owner) |> dispatch)
                                        ] [
                                            i [ classes [ FontAwesome.fa; FontAwesome.faTrashAlt ] ] []
                                        ]
                                    ]
                                ]
                            )
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
                    div [ Class Bootstrap.colMd5; Style [ PaddingLeft "0" ] ] [
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
                    div [ Class Bootstrap.colMd7; Style [ PaddingLeft "0" ] ] [
                        formGroup [
                            Label "Oppervlakte (m²)"
                            Input [
                                Type "number"
                                Min 0
                                Helpers.valueOrDefault state.Lot.Surface
                                OnChange (fun e -> SurfaceChanged e.Value |> dispatch)
                            ]
                            FormError (errorFor (nameof state.Lot.Surface))
                        ]
                    ]
                ]
                div [] [
                    formGroup [
                        Label "Quotiteit (aandeel)"
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

        LotOwnerModal.render 
            {|
                IsOpen = state.ShowingLotOwnerModal
                BuildingId = state.Lot.BuildingId
                LotOwners = state.Lot.Owners |> List.map fst
                OnOk = (fun owners -> LotOwnersChanged owners |> dispatch)
                OnCanceled = (fun _ -> ChangeLotOwnersCanceled |> dispatch) 
            |}
    ]