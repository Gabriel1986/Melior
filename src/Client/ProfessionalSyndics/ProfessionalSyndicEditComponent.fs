module Client.ProfessionalSyndics.ProfessionalSyndicEditComponent

open System
open Fable.React
open Elmish
open Shared.Read
open Client.Organizations

type Message =
    | OrganizationEditComponentMessage of OrganizationEditComponent.Message
    | ErrorsChanged of (string * string) list

type State = {
    ProfessionalSyndic: ProfessionalSyndic
    OrganizationEditComponentState: OrganizationEditComponent.State
}

let init (professionalSyndic: ProfessionalSyndic) =
    let state, cmd = OrganizationEditComponent.init {| Organization = Some professionalSyndic.Organization; Building = None |}
    {
        ProfessionalSyndic = professionalSyndic
        OrganizationEditComponentState = state
    }, cmd |> Cmd.map OrganizationEditComponentMessage

let update (message: Message) (state: State): State * Cmd<Message> =
    match message with
    | OrganizationEditComponentMessage msg ->
        let newState, newCmd = OrganizationEditComponent.update msg state.OrganizationEditComponentState
        { 
            state with 
                OrganizationEditComponentState = newState
                ProfessionalSyndic = { state.ProfessionalSyndic with Organization = newState.Organization }
        }, newCmd |> Cmd.map OrganizationEditComponentMessage
    | ErrorsChanged e ->
        let organizationErrors = e |> List.choose (fun (path: string, error: string) -> 
            if path.StartsWith(nameof state.ProfessionalSyndic.Organization) 
            then Some (String.Join(".", path.Split('.').[1..]), error)
            else None)
        { state with OrganizationEditComponentState = { state.OrganizationEditComponentState with Errors = organizationErrors } }, Cmd.none
        

let view (state: State) (dispatch: Message -> unit) =
    div [] [
        OrganizationEditComponent.view (state.OrganizationEditComponentState) (OrganizationEditComponentMessage >> dispatch)
    ]