module Client.Users.UsersPage

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
    CurrentUser: User
    SelectedListItems: User list
    SelectedTab: Tab
    LoadingListItems: bool
    ListItems: User list
}
and Tab =
    | List
    | Details of User
    | New
type Msg =
    | AddDetailTab of User
    | RemoveDetailTab of User
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: User list * selectedListItemId: Guid option
    | RemoveListItem of User
    | ListItemRemoved of Result<User, DeleteUserError>
    | Created of User
    | Edited of User

type UsersPageProps = {|
    CurrentUser: User
    UserId: Guid option
|}

type SortableUserAttribute =
    | EmailAddress
    | DisplayName
    | PreferredLanguageCode
    | NbBuildings
    member me.ToString' () =
        match me with
        | EmailAddress -> "E-mail"
        | DisplayName -> "Naam"
        | PreferredLanguageCode -> "Taal"
        | NbBuildings -> "#Rollen"
    member me.StringValueOf': User -> string =
        match me with
        | EmailAddress -> (fun li -> li.EmailAddress)
        | DisplayName -> (fun li -> string li.DisplayName)
        | PreferredLanguageCode -> (fun li -> li.PreferredLanguageCode)
        | NbBuildings -> (fun li -> li.Roles |> List.collect (fun r -> r.BuildingIds ()) |> List.distinct |> List.length |> string)
    member me.Compare': User -> User -> int =
        fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ DisplayName; EmailAddress; PreferredLanguageCode; NbBuildings ]
    interface ISortableAttribute<User> with
        member me.ToString = me.ToString'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member me.IsFilterable = true

let init (props: UsersPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        SelectedListItems = []
        SelectedTab = List
        ListItems = []
        LoadingListItems = true
    }
    
    let cmd =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetUsers)
            ()
            (fun users -> Loaded (users, props.UserId))
            RemotingError
    state, cmd

let update (msg: Msg) (state: State): State * Cmd<Msg> =   
    match msg with
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.UserId = listItem.UserId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.DisplayName)
        { state with SelectedListItems = newlySelectedItems; SelectedTab = Details listItem }
        , Routing.navigateToPage (Routing.Page.UserDetails listItem.UserId)
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.UserId <> listItem.UserId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }
        , Routing.navigateToPage Routing.Page.UserList
    | SelectTab tab ->
        { state with SelectedTab = tab }, Cmd.none
    | Loaded (users, selectedUserId) ->
        let newState = { state with ListItems = users; LoadingListItems = false }
        let cmd =
            match selectedUserId with
            | Some selectedUserId ->
                let selectedListItem = users |> List.tryFind (fun listItem -> listItem.UserId = selectedUserId)
                match selectedListItem with
                | Some selected -> AddDetailTab selected |> Cmd.ofMsg
                | None -> Cmd.none
            | None -> Cmd.none
        newState, cmd
    | RemoveListItem user ->
        let cmd =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi().DeleteUser)
                user.UserId
                (fun r -> r |> Result.map (fun _ -> user) |> ListItemRemoved)
                RemotingError

        let newSelection =
            state.SelectedListItems |> List.filter (fun selected -> selected.UserId <> user.UserId)

        let newItems =
            state.ListItems |> List.filter (fun item -> item.UserId <> user.UserId)

        { state with SelectedListItems = newSelection; ListItems = newItems }, cmd
    | ListItemRemoved result ->
        match result with
        | Ok _ -> 
            state, Cmd.none
        | Error DeleteUserError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om deze professionele syndicus te verwijderen"
        | Error DeleteUserError.NotFound ->
            printf "Could not delete the professional syndic, it was not found in the DB, somehow..."
            state, Cmd.none
    | RemotingError e ->
        state, showGenericErrorModalCmd e
    | Created user ->
        let listItem = user
        let newListItems = listItem :: state.ListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems }, Cmd.none
    | Edited user ->
        let listItem = user
        let newListItems = state.ListItems |> List.map (fun li -> if li.UserId = listItem.UserId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.UserId = listItem.UserId then listItem else li)
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
        let list (state: State) =
            SortableTable.render 
                {|
                    ListItems = state.ListItems
                    DisplayAttributes = SortableUserAttribute.All
                    IsSelected = None
                    OnSelect = None
                    IsEditable = None
                    OnEdit = Some (AddDetailTab >> dispatch)
                    IsDeletable = None
                    OnDelete = Some (RemoveListItem >> dispatch)
                    Key = "UsersPageTable"
                |}

        div [ Class Bootstrap.colMd12 ] [
            div [ classes [ Bootstrap.nav; Bootstrap.navTabs ] ] [
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle List); OnClick (fun _ -> SelectTab List |> dispatch) ] 
                        [ str "Overzicht" ] 
                ]
                for selected in state.SelectedListItems do
                    yield li [ Class Bootstrap.navItem ] [
                        a 
                            [ Class (determineNavItemStyle (Details selected)); OnClick (fun _ -> SelectTab (Details selected) |> dispatch) ] 
                            [ str selected.DisplayName ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe professionele syndicus" ]
                ]
            ]

            match state.SelectedTab with
            | List -> list state
            | Details listItem -> 
                UserDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        CreateOrUpdate = UserDetails.CreateOrUpdate.Update listItem
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
            | New ->
                UserDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        CreateOrUpdate = UserDetails.CreateOrUpdate.Create (Guid.NewGuid())
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
        ]
    ]

let render (props: UsersPageProps) =
    React.elmishComponent ("UsersPage", init props, update, view)