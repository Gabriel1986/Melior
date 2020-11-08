module Client.Owners.OwnerEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Shared.Read
open Client.Components

type Message =
    | IsResidentChanged of bool
    | PersonEditComponentMessage of PersonEditComponent.Message
    | ErrorsChanged of (string * string) list

type State = {
    Owner: Owner
    PersonEditComponentState: PersonEditComponent.State
}

let init (owner: Owner) =
    let state, cmd = PersonEditComponent.init (Some owner.Person)

    {
        Owner = owner
        PersonEditComponentState = state
    }, cmd |> Cmd.map PersonEditComponentMessage

let update (message: Message) (state: State): State * Cmd<Message> =
    match message with
    | IsResidentChanged x ->
        { state with Owner = { state.Owner with IsResident = x } }, Cmd.none
    | PersonEditComponentMessage msg ->
        let newState, newCmd = PersonEditComponent.update msg state.PersonEditComponentState
        { 
            state with 
                PersonEditComponentState = newState
                Owner = { state.Owner with Person = newState.Person }
        }, newCmd |> Cmd.map PersonEditComponentMessage
    | ErrorsChanged e ->
        let personErrors = e |> List.choose (fun (path: string, error: string) -> 
            if path.StartsWith(nameof state.Owner.Person) 
            then Some (String.Join(".", path.Split('.').[1..]), error)
            else None)
        { state with PersonEditComponentState = { state.PersonEditComponentState with Errors = personErrors } }, Cmd.none
        

let view (state: State) (dispatch: Message -> unit) =
    let yesNo = [
        {
            Id = "ownerIsResident"
            Key = "IsResident"
            Label = "Ja"
            IsSelected = state.Owner.IsResident
            OnClick = (fun _ -> IsResidentChanged true |> dispatch)
        }
        {
            Id = "ownerIsNotResident"
            Key = "IsNotResident"
            Label = "Nee"
            IsSelected = not state.Owner.IsResident
            OnClick = (fun _ -> IsResidentChanged false |> dispatch)
        }
    ]

    let inColumn x = div [ Class Bootstrap.col ] [ x ]

    div [] [
        div [ classes [ Bootstrap.row; "full-width" ] ] [
            formGroup [ 
                Label "Bewoner"
                Radio {
                    Inline = true
                    RadioButtons = yesNo
                }
            ]
            |> inColumn
        ]
        PersonEditComponent.view (state.PersonEditComponentState) (PersonEditComponentMessage >> dispatch) {| ShowAddresses = true; ShowBankAccounts = true |}
    ]