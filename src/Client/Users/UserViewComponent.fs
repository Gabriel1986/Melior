module Client.Users.UserViewComponent

open System
open Fable.React
open Fable.React.Props
open Fable.Core
open Shared.Read
open Client
open Client.ClientStyle
open Client.ClientStyle.Helpers

module SubComponents =
    let renderBuildings (buildings: BuildingListItem list) (key: string) =
        SortableTable.render 
            {|
                ListItems = (buildings |> List.sortBy (fun b -> b.Code))
                DisplayAttributes = Buildings.BuildingsPage.SortableAttribute.All
                IsSelected = None
                OnSelect = None
                IsEditable = None
                OnEdit = None
                IsDeletable = None
                OnDelete = None
                Key = key
            |}

    let renderProfessionalSyndics (proSyndics: ProfessionalSyndicListItem list) (key: string) =
        SortableTable.render
            {|
                ListItems = (proSyndics |> List.sortBy (fun p -> p.Name))
                DisplayAttributes = ProfessionalSyndics.ProfessionalSyndicsPage.SortableProfessionalSyndicListItemAttribute.All
                IsSelected = None
                OnSelect = None
                IsEditable = None
                OnEdit = None
                IsDeletable = None
                OnDelete = None
                Key = key
            |}

    module BuildingBasedRoleViewComponent =
        type BuildingBasedRoleViewComponentProps = {| Title: string; BuildingIds: Guid list |}

        type State =
            | Loading
            | Loaded of BuildingListItem list

        let view (props: BuildingBasedRoleViewComponentProps) =
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
                        h4 [] [ str props.Title ]
                    ]
                    match componentState.current with
                    | Loading ->
                        div [] [ str "Gebouwen worden geladen" ]
                    | Loaded [] ->
                        div [] [ str "Er werden geen gebouwen gevonden" ]
                    | Loaded buildings ->
                        renderBuildings buildings (string buildings.Length)
                ]
            ]

        let render =          
            FunctionComponent.Of ((fun (props: BuildingBasedRoleViewComponentProps) -> view props), memoizeWith = memoEqualsButFunctions)

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
                        h4 [] [ str "Professionele syndicus rollen" ]
                    ]
                    match componentState.current with
                    | Loading ->
                        div [] [ str "Kantoren worden geladen" ]
                    | Loaded [] ->
                        div [] [ str "Er werden geen kantoren gevonden gevonden" ]
                    | Loaded professionalSyndics ->
                        renderProfessionalSyndics professionalSyndics (string professionalSyndics.Length)
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

        yield fieldset [] [
            legend [] [ h3 [] [ str "Rollen" ] ]
            div [ Class Bootstrap.mt2 ] [
                SubComponents.BuildingBasedRoleViewComponent.render {| Title = "Gebruikersrollen"; BuildingIds = user.Roles |> List.collect (function | UserRole buildingIds -> buildingIds | _ -> []) |}
            ]
            div [ Class Bootstrap.mt2 ] [
                SubComponents.BuildingBasedRoleViewComponent.render {| Title = "Eigenaar syndicus rollen"; BuildingIds = user.Roles |> List.collect (function | SyndicRole buildingIds -> buildingIds | _ -> []) |}
            ]
            div [ Class Bootstrap.mt2 ] [
                SubComponents.ProfessionalSyndicRoleComponent.render {| OrganizationIds = user.Roles |> List.collect (function | ProfessionalSyndicRole (orgId, _) -> [ orgId ] | _ -> []) |}
            ]
        ]
    ]

let render =
    FunctionComponent.Of ((fun (props: {| User: User |}) -> view props.User), memoizeWith = equalsButFunctions)