module Client.Users.UserEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Client.Library
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Shared.Read
open Shared.Library

type Message =
    | NameChanged of string
    | EmailChanged of string
    | LanguageChanged of string
    | TwoFacAuthenticationChanged of bool
    | TwoFacAuthenticationChangedConfirmed of bool
    | IsSysAdminChanged of string
    | UserRoleChanged of BuildingId list
    | SyndicRoleChanged of BuildingId list
    | ProfessionalSyndicRoleChanged of proSyndicId: Guid
    | NoOp
    
type State = {
    User: User
    ShowingBuildingSelectionModal: bool
    Errors: (string * string) list
}

let init (user: User) =
    {
        User = user
        ShowingBuildingSelectionModal = false
        Errors = []
    }, Cmd.none

let update (message: Message) (state: State): State * Cmd<Message> =
    let changeUser f =
        { state with User = f state.User }

    let removeError errorName state =
        { state with Errors = (state.Errors |> List.filter (fun (path, e) -> path <> errorName)) }

    match message with
    | NameChanged x ->
        changeUser (fun l -> { l with DisplayName = x })
        |> removeError (nameof state.User.DisplayName), Cmd.none
    | EmailChanged x ->
        changeUser (fun l -> { l with EmailAddress = x })
        |> removeError (nameof state.User.EmailAddress), Cmd.none
    | LanguageChanged x ->
        changeUser (fun l -> { l with PreferredLanguageCode = x })
        |> removeError (nameof state.User.PreferredLanguageCode), Cmd.none
    | TwoFacAuthenticationChanged x -> 
        state, showConfirmationModal ("", "", (fun () -> TwoFacAuthenticationChangedConfirmed x), (fun () -> NoOp))

let inColomn columnClass x = div [ Class columnClass ] [ x ]

//let view (state: State) (dispatch: Message -> unit) =
//    let errorFor (path: string) =
//        state.Errors |> List.tryPick (fun (p, error) -> if p = path then Some error else None)

//    let toOption (lotType: LotType): FormSelectOption = {
//        Key = string lotType
//        Label = string lotType
//        IsSelected = state.Lot.LotType = lotType
//    }

//    let lotTypeSelection: FormSelect = {
//        Identifier = string state.Lot.LotId
//        OnChanged = (fun e ->
//            let lotType = LotType.OfString e     
//            LotTypeChanged lotType |> dispatch
//        )
//        Options = 
//            [ Appartment; Studio; ParkingSpace; CommercialProperty; Garage; Storage; LotType.Other ]
//            |> List.map toOption
//    }

//    div [] [
//        div [ Class Bootstrap.row ] [
//            formGroup [ 
//                Label "Code"
//                Input [ 
//                    Type "text"
//                    MaxLength 16.0
//                    Helpers.valueOrDefault state.Lot.Code
//                    OnChange (fun e -> CodeChanged e.Value |> dispatch)
//                ]
//                FormError (errorFor (nameof state.Lot.Code))
//            ]
//            |> inColomn Bootstrap.col4

//            formGroup [ 
//                Label "Type"
//                Select lotTypeSelection
//            ]
//            |> inColomn Bootstrap.col
//        ]
//        div [ Class Bootstrap.row ] [
//            div [] [
//                div [] [
//                    formGroup [
//                        Label "Verdieping"
//                        Input [
//                            Type "number"
//                            Min -20
//                            Max 50
//                            Helpers.valueOrDefault state.Lot.Floor
//                            OnChange (fun e -> FloorChanged e.Value |> dispatch)
//                        ]
//                        FormError (errorFor (nameof state.Lot.Floor))
//                    ]
//                ]
//                div [] [
//                    formGroup [
//                        Label "Oppervlakte (m²)"
//                        Input [
//                            Type "number"
//                            Min 0
//                            Helpers.valueOrDefault state.Lot.Surface
//                            OnChange (fun e -> SurfaceChanged e.Value |> dispatch)
//                        ]
//                        FormError (errorFor (nameof state.Lot.Surface))
//                    ]
//                ]
//            ]
//            |> inColomn Bootstrap.col4
//            formGroup [ 
//                Label "Omschrijving"
//                TextArea [
//                    Rows 4
//                    Helpers.valueOrDefault state.Lot.Description
//                    OnChange (fun e -> DescriptionChanged e.Value |> dispatch)
//                ] 
//            ]
//            |> inColomn Bootstrap.col
//        ]

//        renderEditLotOwners state.Lot.Owners dispatch
//        |> inColomn Bootstrap.row

//        LotOwnerModal.render 
//            {|
//                IsOpen = state.ShowingLotOwnerModal
//                BuildingId = state.Lot.BuildingId
//                LotOwners = state.Lot.Owners |> List.map fst
//                OnOk = (fun owners -> LotOwnersChanged owners |> dispatch)
//                OnCanceled = (fun _ -> ChangeLotOwnersCanceled |> dispatch) 
//            |}
//    ]