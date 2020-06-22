module Client.Organizations.ContactPersonEditComponent

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Client.Components
open Shared.Read
open Client.ClientStyle.Helpers

type State = {
    ContactPerson: ContactPerson
    PersonEditComponentState: PersonEditComponent.State
}

type Message =
    | RoleWithinOrganizationChanged of string
    | PersonEditComponentMessage of PersonEditComponent.Message

let init (contactPerson: ContactPerson) =
    let componentState, componentCmd = PersonEditComponent.init (Some contactPerson.Person)
    { ContactPerson = contactPerson; PersonEditComponentState = componentState }, componentCmd |> Cmd.map PersonEditComponentMessage

let update (message: Message) (state: State): State * Cmd<Message> =
    let changeContactPerson fChange =
        { state with ContactPerson = fChange state.ContactPerson }

    match message with
    | RoleWithinOrganizationChanged newRole ->
        changeContactPerson (fun cp -> { cp with RoleWithinOrganization = newRole }), Cmd.none
    | PersonEditComponentMessage msg ->
        let componentState, componentCmd = PersonEditComponent.update msg (state.PersonEditComponentState)
        let newState = changeContactPerson (fun cp -> { cp with Person = componentState.Person })
        { newState with PersonEditComponentState = componentState }, componentCmd |> Cmd.map PersonEditComponentMessage

let view (state: State) (dispatch: Message -> unit) =
    div [] [
        formGroup [
            Label "Rol"
            Input [
                Type "Text"
                MaxLength 32.0
                Helpers.valueOrDefault state.ContactPerson.RoleWithinOrganization
                OnChange (fun e -> RoleWithinOrganizationChanged e.Value |> dispatch)
            ]
        ]
        PersonEditComponent.view (state.PersonEditComponentState) (PersonEditComponentMessage >> dispatch)
    ]