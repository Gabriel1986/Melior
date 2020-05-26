﻿module Client.Organizations.OrganizationsPage

open System
open Elmish
open Fable
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents

open Shared.Domain
open Shared.Remoting
open Client
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.SortableTable

type State = {
    CurrentUser: CurrentUser
    CurrentBuilding: BuildingListItem
    SelectedListItems: OrganizationListItem list
    SelectedTab: Tab
    LoadingListItems: bool
    ListItems: OrganizationListItem list
}
and Tab =
    | List
    | Details of OrganizationListItem
    | New
type Msg =
    | AddDetailTab of OrganizationListItem
    | RemoveDetailTab of OrganizationListItem
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: OrganizationListItem list * selectedListItemId: Guid option
    | RemoveListItem of OrganizationListItem
    | ListItemRemoved of Result<OrganizationListItem, AuthorizationError>
    | Created of Organization
    | Edited of Organization

type OrganizationPageProps = {|
    CurrentUser: CurrentUser
    CurrentBuilding: BuildingListItem
    OrganizationId: Guid option
|}

type SortableOrganizationListItemAttribute =
    | OrganizationNumber
    | Name
    | Type
    member me.ToString' () =
        match me with
        | OrganizationNumber -> "Ondernemingsnr."
        | Name -> "Naam"
        | Type -> "Type"
    member me.StringValueOf': OrganizationListItem -> string =
        match me with
        | OrganizationNumber -> (fun li -> string li.OrganizationNumber)
        | Name -> (fun li -> string li.Name)
        | Type -> (fun li -> li.OrganizationType)
    member me.Compare': OrganizationListItem -> OrganizationListItem -> int =
        match me with
        | _     -> 
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ OrganizationNumber; Name; Type ]
    interface ISortableAttribute<OrganizationListItem> with
        member me.ToString = me.ToString'
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi

let init (props: OrganizationPageProps) =
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
            (Remoting.getRemotingApi().GetOrganizations)
            {| BuildingId = props.CurrentBuilding.BuildingId |}
            (fun owners -> Loaded (owners, props.OrganizationId))
            RemotingError
    state, cmd

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let toListItem (organization: Organization): OrganizationListItem = {
        OrganizationId = organization.OrganizationId
        OrganizationNumber = organization.OrganizationNumber
        OrganizationType = organization.OrganizationType.Name
        Name = organization.Name
    }

    match msg with
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.OrganizationId = listItem.OrganizationId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.Name)
        { state with SelectedListItems = newlySelectedItems; SelectedTab = Details listItem },
        Routing.navigateToPage (Routing.Page.OrganizationDetails { BuildingId = state.CurrentBuilding.BuildingId;  DetailId = listItem.OrganizationId })
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.OrganizationId <> listItem.OrganizationId)
        
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, 
        Cmd.none
    | SelectTab tab ->
        { state with SelectedTab = tab }, Cmd.none
    | Loaded (owners, selectedOrgId) ->
        let newState = { state with ListItems = owners; LoadingListItems = false }
        let cmd =
            match selectedOrgId with
            | Some selectedOrgId ->
                let selectedListItem = owners |> List.tryFind (fun listItem -> listItem.OrganizationId = selectedOrgId)
                match selectedListItem with
                | Some selected -> AddDetailTab selected |> Cmd.ofMsg
                | None -> Cmd.none
            | None -> Cmd.none
        newState, cmd
    | RemoveListItem organization ->
        let cmd =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi().DeleteOrganization)
                organization.OrganizationId
                (fun r -> r |> Result.map (fun _ -> organization) |> ListItemRemoved)
                RemotingError

        let newSelection =
            state.SelectedListItems |> List.filter (fun selected -> selected.OrganizationNumber <> organization.OrganizationNumber)

        let newItems =
            state.ListItems |> List.filter (fun item -> item.OrganizationNumber <> organization.OrganizationNumber)

        { state with SelectedListItems = newSelection; ListItems = newItems }, 
        cmd
    | ListItemRemoved result ->
        match result with
        | Ok _ -> state, Cmd.none
        | Error e -> //TODO...
            state, Cmd.none
    | RemotingError e ->
        //TODO.
        state, Cmd.none
    | Created organization ->
        let listItem = toListItem organization
        let newListItems = listItem :: state.ListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems },
        Cmd.none
    | Edited organization ->
        let listItem = toListItem organization
        let newListItems = state.ListItems |> List.map (fun li -> if li.OrganizationNumber = organization.OrganizationNumber then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.OrganizationNumber = organization.OrganizationNumber then listItem else li)
        
        { state with ListItems = newListItems; SelectedListItems = newSelectedListItems }, 
        Cmd.none

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
            div [ classes [ Bootstrap.nav; Bootstrap.flexMdColumn; Bootstrap.navPills ] ] [
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle List); OnClick (fun _ -> SelectTab List |> dispatch) ] 
                        [ str "Overview" ] 
                ]
                for selected in state.SelectedListItems do
                    yield li [ Class Bootstrap.navItem ] [
                        a 
                            [ Class (determineNavItemStyle (Details selected)); OnClick (fun _ -> SelectTab (Details selected) |> dispatch) ] 
                            [ str (sprintf "%s (%A)" selected.Name selected.OrganizationNumber) ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe organizatie" ]
                ]
            ]
        ]
              
        let list (state: State) =
            SortableTable.render 
                {|
                    ListItems = state.ListItems
                    DisplayAttributes = SortableOrganizationListItemAttribute.All
                    ExtraColumnHeaders = [ 
                        th [] []; 
                        th [] [] 
                    ]
                    ExtraColumns = (fun li -> 
                        seq {
                            td [] [ 
                                a 
                                    [ 
                                        classes [ Bootstrap.textPrimary; "pointer" ]
                                        OnClick (fun _ -> AddDetailTab li |> dispatch) 
                                    ] 
                                    [
                                        i [ classes [ FontAwesome.fa; FontAwesome.faExternalLinkAlt ] ] []
                                    ] 
                            ]
                            td [] [ 
                                a 
                                    [
                                        classes [ Bootstrap.textDanger; "pointer" ]
                                        OnClick (fun _ -> RemoveListItem li |> dispatch) 
                                    ] 
                                    [
                                        i [ classes [ FontAwesome.far; FontAwesome.faTrashAlt ] ] []
                                    ] 
                            ]
                        }
                    )
                    Key = "OrganizationsPageTable"
                |}

        div [ Class Bootstrap.col ] [
            match state.SelectedTab with
            | List -> list state
            | Details listItem -> 
                OrganizationDetails.render 
                    {| 
                        CurrentUser = state.CurrentUser 
                        CurrentBuilding = state.CurrentBuilding
                        Identifier = listItem.OrganizationNumber
                        IsNew = false
                        NotifyCreated = fun b -> dispatch (Created b)
                        NotifyEdited = fun b -> dispatch (Edited b)
                    |}
            | New ->
                OrganizationDetails.render 
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

let render (props: OrganizationPageProps) =
    React.elmishComponent ("OrganizationsPage", init props, update, view)