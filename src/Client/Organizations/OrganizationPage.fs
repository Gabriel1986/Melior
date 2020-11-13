module Client.Organizations.OrganizationsPage

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
    SelectedListItems: OrganizationListItem list
    SelectedTab: Tab
    LoadingListItems: bool
    ListItems: OrganizationListItem list
}
and Tab =
    | List
    | Details of organizationId: Guid
    | New
type Msg =
    | AddDetailTab of OrganizationListItem
    | RemoveDetailTab of OrganizationListItem
    | SelectTab of Tab
    | RemotingError of exn
    | Loaded of listItems: OrganizationListItem list * selectedListItemId: Guid option
    | RemoveListItem of OrganizationListItem
    | ConfirmRemoveListItem of OrganizationListItem
    | ListItemRemoved of Result<OrganizationListItem, DeleteOrganizationError>
    | Created of Organization
    | Edited of Organization
    | NoOp

type OrganizationPageProps = {|
    CurrentUser: User
    CurrentBuilding: BuildingListItem
    OrganizationId: Guid option
|}

type SortableOrganizationListItemAttribute =
    | Type
    | Name
    | VatOrOrgNumber
    override me.ToString () =
        match me with
        | Type -> "Type"
        | Name -> "Naam"
        | VatOrOrgNumber -> "BTW / Ondernemingsnr."
    member me.ReactElementFor': OrganizationListItem -> ReactElement =
        fun li -> str (me.StringValueOf' li)
    member me.StringValueOf': OrganizationListItem -> string =
        match me with
        | Type -> (fun li -> String.Join(", ", li.OrganizationTypeNames))
        | Name -> (fun li -> string li.Name)
        | VatOrOrgNumber -> (fun li -> li.VatNumber |> Option.orElse li.OrganizationNumber |> Option.defaultValue "")
    member me.Compare': OrganizationListItem -> OrganizationListItem -> int =
        match me with
        | _     -> 
            fun li otherLi -> (me.StringValueOf' li).CompareTo(me.StringValueOf' otherLi)
    static member All = [ Type; Name; VatOrOrgNumber ]
    interface ISortableAttribute<OrganizationListItem> with
        member me.ReactElementFor = me.ReactElementFor'
        member _.ExtraHeaderAttributes = Seq.empty
        member me.StringValueOf = me.StringValueOf'
        member me.Compare li otherLi = me.Compare' li otherLi
        member _.IsFilterable = true

let init (props: OrganizationPageProps) =
    let state = { 
        CurrentUser = props.CurrentUser
        CurrentBuilding = props.CurrentBuilding
        SelectedListItems = []
        SelectedTab =
            match props.OrganizationId with
            | Some orgId -> Tab.Details orgId
            | None -> Tab.List
        ListItems = []
        LoadingListItems = true
    }
    
    let cmd =
        Cmd.OfAsync.either
            (Remoting.getRemotingApi().GetOrganizations)
            props.CurrentBuilding.BuildingId
            (fun owners -> Loaded (owners, props.OrganizationId))
            RemotingError
    state, cmd

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let toListItem (organization: Organization): OrganizationListItem = {
        OrganizationId = organization.OrganizationId
        BuildingId = organization.BuildingId
        VatNumber = organization.VatNumber
        OrganizationNumber = organization.OrganizationNumber
        OrganizationTypeNames = organization.OrganizationTypes |> List.map (fun t -> t.Name)
        Name = organization.Name
        Address = organization.Address
        BankAccounts = organization.BankAccounts
    }

    match msg with
    | AddDetailTab listItem ->
        let newlySelectedItems = 
            if state.SelectedListItems |> List.exists (fun li -> li.OrganizationId = listItem.OrganizationId)
            then state.SelectedListItems
            else listItem::state.SelectedListItems
            |> List.sortBy (fun li -> li.Name)

        let updatedState = { state with SelectedListItems = newlySelectedItems }
        match state.SelectedTab with
        | Details detailId when detailId = listItem.OrganizationId ->
            updatedState, Cmd.none
        | _ ->
            { updatedState with SelectedTab = Details listItem.OrganizationId },
            Routing.navigateToPage (Routing.Page.OrganizationDetails { BuildingId = state.CurrentBuilding.BuildingId;  DetailId = listItem.OrganizationId })
    | RemoveDetailTab listItem ->
        let updatedTabs = 
            state.SelectedListItems 
            |> List.filter (fun li -> li.OrganizationId <> listItem.OrganizationId)
        
        { state with SelectedListItems = updatedTabs; SelectedTab = List }, 
        Routing.navigateToPage (Routing.Page.OrganizationList { BuildingId = state.CurrentBuilding.BuildingId })
    | SelectTab tab ->
        let buildingId = state.CurrentBuilding.BuildingId
        let cmd =
            match tab with
            | List -> Routing.navigateToPage (Routing.Page.OrganizationList { BuildingId = buildingId })
            | Details organizationId -> Routing.navigateToPage (Routing.Page.OrganizationDetails { BuildingId = buildingId; DetailId = organizationId })
            | New -> Routing.navigateToPage (Routing.Page.OrganizationList { BuildingId = buildingId })
        { state with SelectedTab = tab }, cmd
    | Loaded (organizations, selectedOrgId) ->
        let newState = { state with ListItems = organizations; LoadingListItems = false }
        let cmd =
            match selectedOrgId with
            | Some selectedOrgId ->
                let selectedListItem = organizations |> List.tryFind (fun listItem -> listItem.OrganizationId = selectedOrgId)
                match selectedListItem with
                | Some selected -> AddDetailTab selected |> Cmd.ofMsg
                | None -> Cmd.none
            | None -> Cmd.none
        newState, cmd
    | RemoveListItem organization ->
        state, 
            showConfirmationModal
                {|
                    Title = "Leverancier verwijderen?"
                    Message = sprintf "Bent u er zeker van dat u %s wilt verwijderen?" organization.Name
                    OnConfirmed = fun () -> ConfirmRemoveListItem organization
                    OnDismissed = fun () -> NoOp
                |}
    | ConfirmRemoveListItem organization ->
        let cmd =
            Cmd.OfAsync.either
                (Remoting.getRemotingApi().DeleteOrganization)
                (organization.BuildingId, organization.OrganizationId)
                (fun r -> r |> Result.map (fun _ -> organization) |> ListItemRemoved)
                RemotingError

        let newSelection =
            state.SelectedListItems |> List.filter (fun selected -> selected.OrganizationId <> organization.OrganizationId)

        let newItems =
            state.ListItems |> List.filter (fun item -> item.OrganizationId <> organization.OrganizationId)

        { state with SelectedListItems = newSelection; ListItems = newItems }, 
        cmd
    | ListItemRemoved result ->
        match result with
        | Ok _ -> 
            state, showSuccessToastCmd "De leverancier is verwijderd"
        | Error DeleteOrganizationError.AuthorizationError ->
            state, showErrorToastCmd "U heeft geen toestemming om een leverancier te verwijderen"
        | Error DeleteOrganizationError.NotFound ->
            printf "The organization couldn't be found in the DB, somehow?"
            state, Cmd.none
    | RemotingError e ->
        { state with ListItems = []; LoadingListItems = false }, showGenericErrorModalCmd e
    | Created organization ->
        let listItem = toListItem organization
        let newListItems = listItem :: state.ListItems
        let newSelectedListItems = [ listItem ] |> List.append state.SelectedListItems
        
        { state with
            ListItems = newListItems
            SelectedListItems = newSelectedListItems
            SelectedTab = Details listItem.OrganizationId
        }
        , Cmd.batch [
            SelectTab (Details listItem.OrganizationId) |> Cmd.ofMsg
            showSuccessToastCmd "De organisatie is aangemaakt"
        ]
    | Edited organization ->
        let listItem = toListItem organization
        let newListItems = state.ListItems |> List.map (fun li -> if li.OrganizationId = organization.OrganizationId then listItem else li)
        let newSelectedListItems = state.SelectedListItems |> List.map (fun li -> if li.OrganizationId = organization.OrganizationId then listItem else li)
        
        { state with 
            ListItems = newListItems
            SelectedListItems = newSelectedListItems
            SelectedTab = Details listItem.OrganizationId
        }
        , Cmd.batch [
            SelectTab (Details listItem.OrganizationId) |> Cmd.ofMsg
            showSuccessToastCmd "De leverancier is gewijzigd"
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
                        DisplayAttributes = SortableOrganizationListItemAttribute.All
                        IsSelected = None
                        OnSelect = None
                        IsEditable = None
                        OnEdit = Some (AddDetailTab >> dispatch)
                        IsDeletable = None
                        OnDelete = Some (RemoveListItem >> dispatch)
                        Key = "OrganizationsPageTable"
                    |}
                if state.LoadingListItems then
                    div [ Class Bootstrap.textCenter ] [
                        str "Leveranciers worden geladen..."
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
                            [ Class (determineNavItemStyle (Details selected.OrganizationId)); OnClick (fun _ -> SelectTab (Details selected.OrganizationId) |> dispatch) ] 
                            [ str (sprintf "%s (%A)" selected.Name (selected.VatNumber |> Option.orElse selected.OrganizationNumber |> Option.defaultValue "")) ]
                    ]
                yield li [ Class Bootstrap.navItem ] [
                    a 
                        [ Class (determineNavItemStyle New); OnClick (fun _ -> SelectTab New |> dispatch) ] 
                        [ str "Nieuwe leverancier" ]
                ]
            ]

            div [ Class Bootstrap.tabContent ] [
                match state.SelectedTab with
                | List -> list state
                | Details organizationId -> 
                    OrganizationDetails.render 
                        {| 
                            CurrentUser = state.CurrentUser 
                            CurrentBuilding = state.CurrentBuilding
                            Identifier = organizationId
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
    ]

let render (props: OrganizationPageProps) =
    React.elmishComponent ("OrganizationsPage", init props, update, view)