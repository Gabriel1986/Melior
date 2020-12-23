module Client.Owners.OwnersPage

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
    CurrentBuilding: BuildingListItem
    SelectedListItems: OwnerListItem list
    SelectedTab: Tab
    LoadingListItems: bool
    ListItems: OwnerListItem list
}
and Tab =
    | List
    | Details of ownerId: Guid
    | New
type Msg =
    | AddDetailTab of OwnerListItem
    | RemoveDetailTab of OwnerListItem
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: OwnerListItem list * selectedListItemId: Guid option
    | RemoveListItem of OwnerListItem
    | ConfirmRemoveListItem of OwnerListItem
    | ListItemRemoved of Result<OwnerListItem, DeleteOwnerError>
    | Created of Owner
    | Edited of Owner
    | NoOp

type OwnersPageProps = {|
    CurrentUser: User
    CurrentBuilding: BuildingListItem
    OwnerId: Guid option
|}

type SortableOwnerListItemAttribute =
    | FirstName
    | LastName
    | IsResident
    override me.ToString () =
        match me with
        | FirstName -> "Voornaam"
        | LastName -> "Achternaam"
        | IsResident -> "Bewoner"
    member me.ReactElementFor': OwnerListItem -> ReactElement =
        fun li -> str (me.StringValueOf' li)
    member me.ExtraHeaderAttributes': IHTMLProp seq =
        match me with
        | IsResident -> seq { Style [ Width "100px" ] }
        | _ -> Seq.empty
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
        member me.ReactElementFor = me.ReactElementFor'
        member me.ExtraHeaderAttributes = me.ExtraHeaderAttributes'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true

let init (props: OwnersPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        CurrentBuilding = props.CurrentBuilding
        SelectedListItems = []
        SelectedTab =
            match props.OwnerId with
            | Some ownerId -> Tab.Details ownerId
            | None -> Tab.List
        ListItems = []
        LoadingListItems = true
    }
    
    let cmd =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetOwners)
            state.CurrentBuilding.BuildingId
            (fun owners -> Loaded (owners, props.OwnerId))
            RemotingError
    state, cmd

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.PersonId = listItem.PersonId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.LastName)
            |> List.sortBy (fun li -> li.FirstName)

        let updatedState = { state with SelectedListItems = newlySelectedItems }
        match state.SelectedTab with
        | Details detailId when detailId = listItem.PersonId ->
            updatedState, Cmd.none
        | _ ->
            { updatedState with SelectedTab = Details listItem.PersonId }
            , Routing.navigateToPage (Routing.Page.OwnerDetails { BuildingId = state.CurrentBuilding.BuildingId; DetailId = listItem.PersonId })
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.PersonId <> listItem.PersonId)
        { state with SelectedListItems = updatedTabs; SelectedTab = List }
        , Routing.navigateToPage (Routing.Page.OwnerList { BuildingId = state.CurrentBuilding.BuildingId })
    | SelectTab tab ->
        let buildingId = state.CurrentBuilding.BuildingId
        let cmd =
            match tab with
            | List -> Routing.navigateToPage (Routing.Page.OwnerList { BuildingId = buildingId })
            | Details personId -> Routing.navigateToPage (Routing.Page.OwnerDetails { BuildingId = buildingId; DetailId = personId })
            | New -> Routing.navigateToPage (Routing.Page.OwnerList { BuildingId = buildingId })
        { state with SelectedTab = tab }, cmd
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
        state, 
            showConfirmationModal
                {|
                    Title = "Eigenaar verwijderen?"
                    Message = sprintf "Bent u er zeker van dat u %s wilt verwijderen?" (owner.FullName ())
                    OnConfirmed = fun () -> ConfirmRemoveListItem owner
                    OnDismissed = fun () -> NoOp
                |}
    | ConfirmRemoveListItem owner ->
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
        | Ok _ -> state, showSuccessToastCmd "De eigenaar is verwijderd"
        | Error DeleteOwnerError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om een eigenaar te verwijderen"
        | Error DeleteOwnerError.NotFound ->
            printf "Could not delete the owner, it was not found in the DB... Somehow..."
            state, Cmd.none
    | RemotingError e ->
        { state with ListItems = []; LoadingListItems = false }, showGenericErrorModalCmd e
    | Created owner ->
        let listItem = owner.ToListItem()
        let newListItems = listItem :: state.ListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        { state with 
            ListItems = newListItems
            SelectedListItems = newSelectedListItems 
        }
        , Cmd.batch [
            SelectTab (Details listItem.PersonId) |> Cmd.ofMsg
            showSuccessToastCmd "De eigenaar is aangemaakt"
        ]
    | Edited owner ->
        let listItem = owner.ToListItem()
        let newListItems = state.ListItems |> List.map (fun li -> if li.PersonId = listItem.PersonId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.PersonId = listItem.PersonId then listItem else li)
        { state with 
            ListItems = newListItems
            SelectedListItems = newSelectedListItems 
        }
        , Cmd.batch [
            SelectTab (Details listItem.PersonId) |> Cmd.ofMsg
            showSuccessToastCmd "De eigenaar is gewijzigd"
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
                        DisplayAttributes = SortableOwnerListItemAttribute.All
                        IsSelected = None
                        OnSelect = None
                        IsEditable = None
                        OnEdit = Some (AddDetailTab >> dispatch)
                        IsDeletable = None
                        OnDelete = Some (RemoveListItem >> dispatch)
                        Key = "OwnersPageTable"
                    |}
                if state.LoadingListItems then
                    div [ Class Bootstrap.textCenter ] [
                        str "Eigenaars worden geladen..."
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
                            [ Class (determineNavItemStyle (Details selected.PersonId)); OnClick (fun _ -> SelectTab (Details selected.PersonId) |> dispatch) ] 
                            [ str (sprintf "%s %s" (defaultArg selected.FirstName "") (defaultArg selected.LastName "")) ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe eigenaar" ]
                ]
            ]

            div [ Class Bootstrap.tabContent ] [
                match state.SelectedTab with
                | List -> list state
                | Details personId -> 
                    OwnerDetails.render 
                        {| 
                            CurrentUser = state.CurrentUser 
                            CurrentBuilding = state.CurrentBuilding
                            Identifier = personId
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
    ]
    |> withPageHeader "Eigenaars"

let render (props: OwnersPageProps) =
    React.elmishComponent ("OwnersPage", init props, update, view)