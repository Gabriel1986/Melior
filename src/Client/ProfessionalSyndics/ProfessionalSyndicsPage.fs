module Client.ProfessionalSyndics.ProfessionalSyndicsPage

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Read
open Shared.Remoting

open Client
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.SortableTable
open Client.Library

type State = {
    CurrentUser: CurrentUser
    SelectedListItems: ProfessionalSyndicListItem list
    SelectedTab: Tab
    LoadingListItems: bool
    ListItems: ProfessionalSyndicListItem list
}
and Tab =
    | List
    | Details of ProfessionalSyndicListItem
    | New
type Msg =
    | AddDetailTab of ProfessionalSyndicListItem
    | RemoveDetailTab of ProfessionalSyndicListItem
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: ProfessionalSyndicListItem list * selectedListItemId: Guid option
    | RemoveListItem of ProfessionalSyndicListItem
    | ListItemRemoved of Result<ProfessionalSyndicListItem, AuthorizationError>
    | Created of ProfessionalSyndic
    | Edited of ProfessionalSyndic

type ProfessionalSyndicsPageProps = {|
    CurrentUser: CurrentUser
    ProfessionalSyndicId: Guid option
|}

type SortableProfessionalSyndicListItemAttribute =
    | Name
    | Address
    | EmailAddress
    | TelephoneNumber
    member me.ToString' () =
        match me with
        | Name -> "Naam"
        | Address -> "Adres"
        | EmailAddress -> "E-mail"
        | TelephoneNumber -> "Tel."
    member me.StringValueOf': ProfessionalSyndicListItem -> string =
        match me with
        | Name -> (fun li -> li.Name)
        | Address -> (fun li -> string li.Address)
        | EmailAddress -> (fun li -> defaultArg li.MainEmailAddress "")
        | TelephoneNumber -> (fun li -> defaultArg li.MainTelephoneNumber "")
    member me.Compare': ProfessionalSyndicListItem -> ProfessionalSyndicListItem -> int =
        fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ Name; Address; EmailAddress; TelephoneNumber ]
    interface ISortableAttribute<ProfessionalSyndicListItem> with
        member me.ToString = me.ToString'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi

let init (props: ProfessionalSyndicsPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        SelectedListItems = []
        SelectedTab = List
        ListItems = []
        LoadingListItems = true
    }
    
    let cmd =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetProfessionalSyndics)
            ()
            (fun professionalSyndics -> Loaded (professionalSyndics, props.ProfessionalSyndicId))
            RemotingError
    state, cmd

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let toListItem (professionalSyndic: ProfessionalSyndic): ProfessionalSyndicListItem = {
        OrganizationId = professionalSyndic.Organization.OrganizationId
        Name = professionalSyndic.Organization.Name
        Address = professionalSyndic.Organization.Address
        MainEmailAddress = professionalSyndic.Organization.MainEmailAddress
        MainTelephoneNumber = professionalSyndic.Organization.MainTelephoneNumber
    }

    match msg with
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.OrganizationId = listItem.OrganizationId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.Name)
        { state with SelectedListItems = newlySelectedItems; SelectedTab = Details listItem }, Routing.navigateToPage (Routing.Page.ProfessionalSyndicDetails listItem.OrganizationId)
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.OrganizationId <> listItem.OrganizationId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Cmd.none
    | SelectTab tab ->
        { state with SelectedTab = tab }, Cmd.none
    | Loaded (professionalSyndics, selectedProfessionalSyndicId) ->
        let newState = { state with ListItems = professionalSyndics; LoadingListItems = false }
        let cmd =
            match selectedProfessionalSyndicId with
            | Some selectedProfessionalSyndicId ->
                let selectedListItem = professionalSyndics |> List.tryFind (fun listItem -> listItem.OrganizationId = selectedProfessionalSyndicId)
                match selectedListItem with
                | Some selected -> AddDetailTab selected |> Cmd.ofMsg
                | None -> Cmd.none
            | None -> Cmd.none
        newState, cmd
    | RemoveListItem professionalSyndic ->
        let cmd =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi().DeleteProfessionalSyndic)
                professionalSyndic.OrganizationId
                (fun r -> r |> Result.map (fun _ -> professionalSyndic) |> ListItemRemoved)
                RemotingError

        let newSelection =
            state.SelectedListItems |> List.filter (fun selected -> selected.OrganizationId <> professionalSyndic.OrganizationId)

        let newItems =
            state.ListItems |> List.filter (fun item -> item.OrganizationId <> professionalSyndic.OrganizationId)

        { state with SelectedListItems = newSelection; ListItems = newItems }, cmd
    | ListItemRemoved result ->
        match result with
        | Ok _ -> state, Cmd.none
        | Error AuthorizationError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om deze professionele syndicus te verwijderen"
    | RemotingError e ->
        state, showGenericErrorModalCmd e
    | Created professionalSyndic ->
        let listItem = toListItem professionalSyndic
        let newListItems = listItem :: state.ListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems }, Cmd.none
    | Edited professionalSyndic ->
        let listItem = toListItem professionalSyndic
        let newListItems = state.ListItems |> List.map (fun li -> if li.OrganizationId = listItem.OrganizationId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.OrganizationId = listItem.OrganizationId then listItem else li)
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems }, Cmd.none

let view (state: State) (dispatch: Msg -> unit): ReactElement =
    let determineNavItemStyle (tab: Tab) =
        let extraClasses =
            if state.SelectedTab = tab 
            then
                [ Bootstrap.active ]
            else
                []
        String.Join(" ", Bootstrap.navLink::extraClasses)

    div [ Class Bootstrap.row ] [
        div [ Class Bootstrap.colMd3 ] [
            div [ classes [ Bootstrap.nav; Bootstrap.navTabs; "left-tabs" ] ] [
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle List); OnClick (fun _ -> SelectTab List |> dispatch) ] 
                        [ str "Overzicht" ] 
                ]
                for selected in state.SelectedListItems do
                    yield li [ Class Bootstrap.navItem ] [
                        a 
                            [ Class (determineNavItemStyle (Details selected)); OnClick (fun _ -> SelectTab (Details selected) |> dispatch) ] 
                            [ str selected.Name ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe professionele syndicus" ]
                ]
            ]
        ]
              
        let list (state: State) =
            SortableTable.render 
                {|
                    ListItems = state.ListItems
                    DisplayAttributes = SortableProfessionalSyndicListItemAttribute.All
                    IsSelected = None
                    OnSelect = None
                    OnEdit = Some (AddDetailTab >> dispatch)
                    OnDelete = Some (RemoveListItem >> dispatch)
                    Key = "ProfessionalSyndicsPageTable"
                |}

        div [ Class Bootstrap.colMd9 ] [
            match state.SelectedTab with
            | List -> list state
            | Details listItem -> 
                ProfessionalSyndicDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        Identifier = listItem.OrganizationId
                        IsNew = false
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
            | New ->
                ProfessionalSyndicDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        Identifier = Guid.NewGuid()
                        IsNew = true
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
        ]
    ]

let render (props: ProfessionalSyndicsPageProps) =
    React.elmishComponent ("ProfessionalSyndicsPage", init props, update, view)