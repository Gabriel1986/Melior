module Client.Users.UserViewComponent

open System
open Fable.React
open Fable.Core
open Shared.Read
open Client
open Client.ClientStyle.Helpers

module SubComponents =
    let renderBuildings (buildings: BuildingListItem list) (key: string) =
        SortableTable.render 
            {|
                ListItems = buildings
                DisplayAttributes = Buildings.BuildingsPage.SortableAttribute.All
                IsSelected = None
                OnSelect = None
                IsEditable = None
                OnEdit = None
                IsDeletable = None
                OnDelete = None
                Key = key
            |}

    let renderProfessionalSyndics (proSyndics: ProfessionalSyndicListItem list) =
        SortableTable.render
            {|
                ListItems = proSyndics
                DisplayAttributes = ProfessionalSyndics.ProfessionalSyndicsPage.SortableProfessionalSyndicListItemAttribute.All
                IsSelected = None
                OnSelect = None
                IsEditable = None
                OnEdit = None
                IsDeletable = None
                OnDelete = None
                Key = "ProfessionalSyndicRoles"
            |}

    module UserRoleViewComponent =
        type UserRoleViewComponentProps = {| BuildingIds: Guid list |}

        type State =
            | Loading
            | Loaded of BuildingListItem list

        let view (props: UserRoleViewComponentProps) =
            let componentState = Hooks.useState Loading
            Hooks.useEffect (fun () ->
                async {
                    let! buildings = Client.Remoting.getRemotingApi().GetBuildings (Some props.BuildingIds)
                    componentState.update (Loaded buildings)
                }
                |> Async.StartAsPromise
                |> ignore
            , [| props.BuildingIds |])

            div [] [
                fieldset [] [
                    legend [] [
                        h4 [] [ str "Eigenaar rollen" ]
                    ]
                    match componentState.current with
                    | Loading ->
                        div [] [ str "Gebouwen worden geladen" ]
                    | Loaded buildings ->
                        renderBuildings buildings "UserRoles"
                ]
            ]

        let render =          
            FunctionComponent.Of ((fun (props: UserRoleViewComponentProps) -> view props), memoizeWith = memoEqualsButFunctions)

    module SyndicRoleComponent =
        type SyndicRoleComponentProps = {| BuildingIds: Guid list |}

        type State =
            | Loading
            | Loaded of BuildingListItem list

        let view (props: SyndicRoleComponentProps) =
            let componentState = Hooks.useState Loading
            Hooks.useEffect (fun () ->
                async {
                    let! buildings = Client.Remoting.getRemotingApi().GetBuildings (Some props.BuildingIds)
                    componentState.update (Loaded buildings)
                }
                |> Async.StartAsPromise
                |> ignore
            , [| props.BuildingIds |])

            div [] [
                fieldset [] [
                    legend [] [
                        h4 [] [ str "Eigenaar syndicus rollen" ]
                    ]
                    match componentState.current with
                    | Loading ->
                        div [] [ str "Gebouwen worden geladen" ]
                    | Loaded buildings ->
                        renderBuildings buildings "SyndicRoles"
                ]
            ]

        let render =          
            FunctionComponent.Of ((fun (props: SyndicRoleComponentProps) -> view props), memoizeWith = memoEqualsButFunctions)

    module ProfessionalSyndicRoleComponent =
        type ProfessionalSyndicRoleComponentProps = {| OrganizationIds: Guid list |}

        type State =
            | Loading
            | Loaded of ProfessionalSyndicListItem list

        let view (props: ProfessionalSyndicRoleComponentProps) =
            let componentState = Hooks.useState Loading
            Hooks.useEffect (fun () ->
                async {
                    let! organizations = Client.Remoting.getRemotingApi().GetProfessionalSyndics (Some props.OrganizationIds)
                    componentState.update (Loaded organizations)
                }
                |> Async.StartAsPromise
                |> ignore
            , [| props.OrganizationIds |])

            div [] [
                fieldset [] [
                    legend [] [
                        h4 [] [ str "Eigenaar syndicus rollen" ]
                    ]
                    match componentState.current with
                    | Loading ->
                        div [] [ str "Organisaties worden geladen" ]
                    | Loaded professionalSyndics ->
                        renderProfessionalSyndics professionalSyndics
                ]
            ]

        let render =          
            FunctionComponent.Of ((fun (props: ProfessionalSyndicRoleComponentProps) -> view props), memoizeWith = memoEqualsButFunctions)


let view (user: User) =
    div [] [
        yield readonlyFormElement "Naam" user.DisplayName
        yield readonlyFormElement "E-mail" user.EmailAddress
        yield readonlyFormElement "Taal" user.PreferredLanguageCode
        yield readonlyFormElement "Gebruikt TFA?" (if user.UseTwoFac then "Ja" else "Nee")
        yield readonlyFormElement "System admin?" (if user.IsSysAdmin () then "Ja" else "Nee")

        if user.Roles.Length > 0 then
            yield fieldset [] [
                legend [] [ h2 [] [ str "Rollen" ] ]
                SubComponents.UserRoleViewComponent.render {| BuildingIds = user.Roles |> List.collect (function | UserRole buildingIds -> buildingIds | _ -> []) |}
                SubComponents.SyndicRoleComponent.render {| BuildingIds = user.Roles |> List.collect (function | SyndicRole buildingIds -> buildingIds | _ -> []) |}
                SubComponents.ProfessionalSyndicRoleComponent.render {| OrganizationIds = user.Roles |> List.collect (function | ProfessionalSyndicRole (orgId, _) -> [ orgId ] | _ -> []) |}
            ]
    ]

let render =
    FunctionComponent.Of ((fun (props: {| User: User |}) -> view props.User), memoizeWith = memoEqualsButFunctions)