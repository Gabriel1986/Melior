module Client.Lots.LotEditComponent

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
    | CodeChanged of string
    | LotTypeChanged of LotType
    | DescriptionChanged of string
    | FloorChanged of string
    | SurfaceChanged of string
    | ChangeLotOwner
    | LotOwnerChanged of LotOwner option
    | LotOwnerChangeCanceled

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

    let parseInt str =
        match str |> String.toOption with
        | Some s ->
            let isParsed, parsed = s |> String.filter Char.IsDigit |> Int32.TryParse
            if isParsed then Some parsed else None
        | None ->
            None


    match message with
    | CodeChanged x ->
        changeLot (fun l -> { l with Code = x }), Cmd.none
    | LotTypeChanged x ->
        changeLot (fun l -> { l with LotType = x }), Cmd.none
    | DescriptionChanged x ->
        changeLot (fun l -> { l with Description = x |> String.toOption }), Cmd.none
    | FloorChanged x ->
        changeLot (fun l -> { l with Floor = parseInt x }), Cmd.none
    | SurfaceChanged x ->
        changeLot (fun l -> { l with Surface = parseInt x }), Cmd.none
    | ChangeLotOwner ->
        { state with ShowingLotOwnerModal = true }, Cmd.none
    | LotOwnerChangeCanceled ->
        { state with ShowingLotOwnerModal = false }, Cmd.none
    | LotOwnerChanged newOwner ->
        let newState = changeLot (fun l -> { l with CurrentOwner = newOwner })
        { newState with ShowingLotOwnerModal = false }, Cmd.none


let renderEditLotOwner lotOwner dispatch =
    let ownerName =
        match lotOwner with
        | Some (LotOwner.Owner o)        -> Some o.Person.FullName
        | Some (LotOwner.Organization o) -> Some o.Name
        | None                           -> None

    match ownerName with
    | Some ownerName ->
        div [ Class Bootstrap.formGroup ] [
            label [] [ str "Eigenaar" ]
            div [ Class Bootstrap.formInline ] [
                div [ Class Bootstrap.formGroup ] [
                    label [ Class Bootstrap.mr2 ] [ str ownerName ]
                    button [ 
                        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary; Bootstrap.btnSm; Bootstrap.mr2 ]
                        OnClick (fun _ -> ChangeLotOwner |> dispatch) 
                    ] [ i [ classes [ FontAwesome.fa; FontAwesome.faEdit ] ] [] ]
                    button [
                        classes [ Bootstrap.btn; Bootstrap.btnOutlineDanger; Bootstrap.btnSm ]
                        OnClick (fun _ -> LotOwnerChanged None |> dispatch)
                    ] [ i [ classes [ FontAwesome.fa; FontAwesome.faTrash ] ] [] ]
                ]
            ]
        ]
    | None ->
        div [ Class Bootstrap.formGroup ] [
            button [ 
                classes [ Bootstrap.btn; Bootstrap.btnSecondary ]
                OnClick (fun _ -> ChangeLotOwner |> dispatch) 
            ] [ str "Eigenaar aanduiden" ]
        ]

let inColomn x = div [ Class Bootstrap.col ] [ x ]

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
            let lotType =
                match e with
                | _ when e = string Appartment -> Appartment
                | _ when e = string Studio -> Studio
                | _ when e = string ParkingSpace -> ParkingSpace
                | _ when e = string CommercialProperty -> CommercialProperty
                | _ when e = string Garage -> Garage
                | _ when e = string Storage -> Storage
                | _ -> LotType.Other
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
            |> inColomn

            formGroup [ 
                Label "Type"
                Select lotTypeSelection
            ]
            |> inColomn

            renderEditLotOwner state.Lot.CurrentOwner dispatch
            |> inColomn
        ]
        div [ Class Bootstrap.row ] [
            div [] [
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
                |> inColomn
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
                |> inColomn
            ]
            |> inColomn
            formGroup [ 
                Label "Omschrijving"
                TextArea [
                    Rows 4
                    Helpers.valueOrDefault state.Lot.Description
                    OnChange (fun e -> DescriptionChanged e.Value |> dispatch)
                ] 
            ]
            |> inColomn
        ]

        LotOwnerModal.render 
            {|
                IsOpen = state.ShowingLotOwnerModal
                BuildingId = state.Lot.BuildingId
                LotOwner = state.Lot.CurrentOwner
                OnLotOwnerChanged = (fun owner -> LotOwnerChanged (Some owner) |> dispatch)
                OnCanceled = (fun _ -> LotOwnerChangeCanceled |> dispatch) 
            |}
    ]