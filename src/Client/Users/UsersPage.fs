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
    | Details of userId: Guid
    | New
type Msg =
    | AddDetailTab of User
    | RemoveDetailTab of User
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: User list * selectedListItemId: Guid option
    | RemoveListItem of User
    | ConfirmRemoveListItem of User
    | ListItemRemoved of Result<User, DeleteUserError>
    | Created of User
    | Edited of User
    | NoOp

type UsersPageProps = {|
    CurrentUser: User
    UserId: Guid option
|}

type SortableUserAttribute =
    | EmailAddress
    | DisplayName
    | PreferredLanguageCode
    | IsSysAdmin
    | NbUserRoles
    | NbSyndicRoles
    | NbProSyndicRoles
    override me.ToString () =
        match me with
        | EmailAddress -> "E-mail"
        | DisplayName -> "Naam"
        | PreferredLanguageCode -> "Taal"
        | IsSysAdmin -> "Admin"
        | NbUserRoles -> "#G"
        | NbSyndicRoles -> "#E"
        | NbProSyndicRoles -> "#P"
    member me.ExtraHeaderAttributes': IHTMLProp seq =
        match me with
        | NbUserRoles -> seq { Title "#Gebruikersrollen" }
        | NbSyndicRoles -> seq { Title "#Eigenaar-syndicus rollen" }
        | NbProSyndicRoles -> seq { Title "#Professioneel syndicus rollen" }
        | _ -> Seq.empty
    member me.ReactElementFor': User -> ReactElement =
        fun li -> str (me.StringValueOf' li)
    member me.StringValueOf': User -> string =
        match me with
        | EmailAddress -> (fun li -> li.EmailAddress)
        | DisplayName -> (fun li -> string li.DisplayName)
        | PreferredLanguageCode -> (fun li -> li.PreferredLanguageCode)
        | IsSysAdmin -> (fun li -> if li.IsSysAdmin() then "Ja" else "Nee")
        | NbUserRoles -> (fun li -> li.Roles |> List.collect (function | UserRole b -> b | _ -> []) |> List.length |> string)
        | NbSyndicRoles -> (fun li -> li.Roles |> List.collect (function | SyndicRole b -> b | _ -> []) |> List.length |> string)
        | NbProSyndicRoles -> (fun li -> li.Roles |> List.collect (function | ProfessionalSyndicRole (org, _) -> [ org ] | _ -> []) |> List.length |> string)
    member me.Compare': User -> User -> int =
        match me with
        | NbUserRoles -> 
            fun li otherLi -> 
                let myNbUserRoles = li.Roles |> List.collect (function | UserRole b -> b | _ -> []) |> List.length
                let otherNbUserRoles = otherLi.Roles |> List.collect (function | UserRole b -> b | _ -> []) |> List.length
                myNbUserRoles - otherNbUserRoles
        | NbSyndicRoles ->
            fun li otherLi ->
                let myNbUserRoles = li.Roles |> List.collect (function | SyndicRole b -> b | _ -> []) |> List.length
                let otherNbUserRoles = otherLi.Roles |> List.collect (function | SyndicRole b -> b | _ -> []) |> List.length
                myNbUserRoles - otherNbUserRoles
        | NbProSyndicRoles ->
            fun li otherLi ->
                let myNbUserRoles = li.Roles |> List.collect (function | ProfessionalSyndicRole (org, _) -> [ org ] | _ -> []) |> List.length
                let otherNbUserRoles = otherLi.Roles |> List.collect (function | ProfessionalSyndicRole (org, _) -> [ org ] | _ -> []) |> List.length
                myNbUserRoles - otherNbUserRoles
        | _ ->
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ DisplayName; EmailAddress; PreferredLanguageCode; IsSysAdmin; NbUserRoles; NbSyndicRoles; NbProSyndicRoles ]
    interface ISortableAttribute<User> with
        member me.ReactElementFor = me.ReactElementFor'
        member me.ExtraHeaderAttributes = me.ExtraHeaderAttributes'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member me.IsFilterable = true

let init (props: UsersPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        SelectedListItems = []
        SelectedTab =
            match props.UserId with
            | Some userId -> Tab.Details userId
            | None -> Tab.List
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

        let updatedState = { state with SelectedListItems = newlySelectedItems }
        match state.SelectedTab with
        | Details detailId when detailId = listItem.UserId ->
            updatedState, Cmd.none
        | _ ->
            { updatedState with SelectedTab = Details listItem.UserId }
            , Routing.navigateToPage (Routing.Page.UserDetails listItem.UserId)
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.UserId <> listItem.UserId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }
        , Routing.navigateToPage Routing.Page.UserList
    | SelectTab tab ->
        let cmd =
            match tab with
            | List -> Routing.navigateToPage (Routing.Page.UserList)
            | Details userId -> Routing.navigateToPage (Routing.Page.UserDetails userId)
            | New -> Routing.navigateToPage (Routing.Page.UserList)
        { state with SelectedTab = tab }, cmd
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
        state, 
            showConfirmationModal
                {|
                    Title = "Gebruiker verwijderen?"
                    Message = sprintf "Bent u er zeker van dat u %s wilt verwijderen?" user.DisplayName
                    OnConfirmed = fun () -> ConfirmRemoveListItem user
                    OnDismissed = fun () -> NoOp
                |}
    | ConfirmRemoveListItem user ->
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
            state, showSuccessToastCmd "De gebruiker is verwijderd"
        | Error DeleteUserError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om deze professionele syndicus te verwijderen"
        | Error DeleteUserError.NotFound ->
            printf "Could not delete the professional syndic, it was not found in the DB, somehow..."
            state, Cmd.none
    | RemotingError e ->
        { state with ListItems = []; LoadingListItems = false }, showGenericErrorModalCmd e
    | Created user ->
        let listItem = user
        let newListItems = listItem :: state.ListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        { state with 
            ListItems = newListItems
            SelectedListItems = newSelectedListItems
        }
        , Cmd.batch [
            SelectTab (Details listItem.UserId) |> Cmd.ofMsg
            showSuccessToastCmd "De gebruiker is aangemaakt"
        ]
    | Edited user ->
        let listItem = user
        let newListItems = state.ListItems |> List.map (fun li -> if li.UserId = listItem.UserId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.UserId = listItem.UserId then listItem else li)
        { state with 
            ListItems = newListItems
            SelectedListItems = newSelectedListItems
        }
        , Cmd.batch [
            SelectTab (Details listItem.UserId) |> Cmd.ofMsg
            showSuccessToastCmd "De gebruiker is gewijzigd"
        ]
    | NoOp ->
        state, Cmd.none

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
            [
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
                if state.LoadingListItems then
                    div [ Class Bootstrap.textCenter ] [
                        str "Gebruikers worden geladen..."
                    ]
                elif state.ListItems |> List.length = 0 then
                    div [ Class Bootstrap.textCenter ] [
                        str "Er werden geen resultaten gevonden..."
                    ]
            ]
            |> fragment []

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
                            [ Class (determineNavItemStyle (Details selected.UserId)); OnClick (fun _ -> SelectTab (Details selected.UserId) |> dispatch) ] 
                            [ str selected.DisplayName ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe gebruiker" ]
                ]
            ]

            div [ Class Bootstrap.tabContent ] [
                match state.SelectedTab with
                | List -> list state
                | Details userId -> 
                    UserDetails.render 
                        {| 
                            CurrentUser = state.CurrentUser 
                            CreateOrUpdate = UserDetails.CreateOrUpdate.Update (userId)
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
    ]
    |> withPageHeader "Gebruikers"

let render (props: UsersPageProps) =
    React.elmishComponent ("UsersPage", init props, update, view, string props.UserId)