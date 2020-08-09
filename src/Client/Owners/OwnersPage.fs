module Client.Owners.OwnersPage

open System
open Elmish
open Elmish.SweetAlert
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
    CurrentBuilding: BuildingListItem
    SelectedListItems: OwnerListItem list
    SelectedTab: Tab
    LoadingListItems: bool
    ListItems: OwnerListItem list
}
and Tab =
    | List
    | Details of OwnerListItem
    | New
type Msg =
    | AddDetailTab of OwnerListItem
    | RemoveDetailTab of OwnerListItem
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: OwnerListItem list * selectedListItemId: Guid option
    | RemoveListItem of OwnerListItem
    | ListItemRemoved of Result<OwnerListItem, DeleteOwnerError>
    | Created of Owner
    | Edited of Owner

type OwnersPageProps = {|
    CurrentUser: User
    CurrentBuilding: BuildingListItem
    PersonId: Guid option
|}

type SortableOwnerListItemAttribute =
    | FirstName
    | LastName
    | IsResident
    member me.ToString' () =
        match me with
        | FirstName -> "Voornaam"
        | LastName -> "Achternaam"
        | IsResident -> "Bewoner"
    member me.StringValueOf': OwnerListItem -> string =
        match me with
        | FirstName -> (fun li -> string li.FirstName)
        | LastName -> (fun li -> string li.LastName)
        | IsResident -> (fun li -> if li.IsResident then "Ja" else "Nee")
    member me.Compare': OwnerListItem -> OwnerListItem -> int =
        match me with
        | IsResident ->
            fun li otherLi -> 
                if li.IsResident = otherLi.IsResident then 0 
                elif li.IsResident && not otherLi.IsResident then 1 else -1

        | _     -> 
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ FirstName; LastName; IsResident ]
    interface ISortableAttribute<OwnerListItem> with
        member me.ToString = me.ToString'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true

let init (props: OwnersPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        CurrentBuilding = props.CurrentBuilding
        SelectedListItems = []
        SelectedTab = List
        ListItems = []
        LoadingListItems = true
    }
    
    let cmd =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetOwners)
            {| BuildingId = state.CurrentBuilding.BuildingId |}
            (fun owners -> Loaded (owners, props.PersonId))
            RemotingError
    state, cmd

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let toListItem (owner: Owner): OwnerListItem = {
        BuildingId = owner.BuildingId
        PersonId = owner.Person.PersonId
        FirstName = owner.Person.FirstName
        LastName = owner.Person.LastName
        IsResident = owner.IsResident
    }

    match msg with
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.PersonId = listItem.PersonId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.LastName)
            |> List.sortBy (fun li -> li.FirstName)
        { state with SelectedListItems = newlySelectedItems; SelectedTab = Details listItem }, Routing.navigateToPage (Routing.Page.OwnerDetails { BuildingId = state.CurrentBuilding.BuildingId; DetailId = listItem.PersonId })
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.PersonId <> listItem.PersonId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, Cmd.none
    | SelectTab tab ->
        { state with SelectedTab = tab }, Cmd.none
    | Loaded (owners, selectedOwnerId) ->
        let newState = { state with ListItems = owners; LoadingListItems = false }
        let cmd =
            match selectedOwnerId with
            | Some selectedOwnerId ->
                let selectedListItem = owners |> List.tryFind (fun listItem -> listItem.PersonId = selectedOwnerId)
                match selectedListItem with
                | Some selected -> AddDetailTab selected |> Cmd.ofMsg
                | None -> Cmd.none
            | None -> Cmd.none
        newState, cmd
    | RemoveListItem owner ->
        let cmd =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi().DeleteOwner)
                (owner.BuildingId, owner.PersonId)
                (fun r -> r |> Result.map (fun _ -> owner) |> ListItemRemoved)
                RemotingError

        let newSelection =
            state.SelectedListItems |> List.filter (fun selected -> selected.PersonId <> owner.PersonId)

        let newItems =
            state.ListItems |> List.filter (fun item -> item.PersonId <> owner.PersonId)

        { state with SelectedListItems = newSelection; ListItems = newItems }, cmd
    | ListItemRemoved result ->
        match result with
        | Ok _ -> state, Cmd.none
        | Error DeleteOwnerError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om een eigenaar te verwijderen"
        | Error DeleteOwnerError.NotFound ->
            printf "Could not delete the owner, it was not found in the DB... Somehow..."
            state, Cmd.none
    | RemotingError e ->
        printf "Error: %A" e
        let alert = SimpleAlert("Er is iets misgegaan bij de communicatie met de server.").Type(AlertType.Error)
        state, SweetAlert.Run(alert)
    | Created owner ->
        let listItem = toListItem owner
        let newListItems = listItem :: state.ListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        let toast = ToastAlert("De eigenaar werd bewaard").Timeout(3000)
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems }, SweetAlert.Run(toast)
    | Edited owner ->
        let listItem = toListItem owner
        let newListItems = state.ListItems |> List.map (fun li -> if li.PersonId = listItem.PersonId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.PersonId = listItem.PersonId then listItem else li)
        let toast = ToastAlert("De eigenaar werd bewaard").Timeout(3000)
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems }, SweetAlert.Run(toast)

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
                            [ str (sprintf "%s %s" (defaultArg selected.FirstName "") (defaultArg selected.LastName "")) ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe eigenaar" ]
                ]
            ]
        ]
              
        let list (state: State) =
            SortableTable.render 
                {|
                    ListItems = state.ListItems
                    DisplayAttributes = SortableOwnerListItemAttribute.All
                    IsSelected = None
                    OnSelect = None
                    OnEdit = Some (AddDetailTab >> dispatch)
                    OnDelete = Some (RemoveListItem >> dispatch)
                    Key = "OwnersPageTable"
                |}

        div [ Class Bootstrap.colMd9 ] [
            match state.SelectedTab with
            | List -> list state
            | Details listItem -> 
                OwnerDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        CurrentBuilding = state.CurrentBuilding
                        Identifier = listItem.PersonId
                        IsNew = false
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
            | New ->
                OwnerDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        CurrentBuilding = state.CurrentBuilding
                        Identifier = Guid.NewGuid()
                        IsNew = true
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
        ]
    ]

let render (props: OwnersPageProps) =
    React.elmishComponent ("OwnersPage", init props, update, view)