module Client.Financial.Settings

open System
open Elmish
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents
open Shared.Read
open Shared.Remoting
open Shared.Library
open Client
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Library

type State = {
    CurrentUser: User
    CurrentBuildingId: Guid
    IsLoadingFinancialYears: bool
    IsLoadingFinancialCategories: bool
    FinancialYears: FinancialYear list
    FinancialCategories: FinancialCategory list
}

type Msg =
    | FinancialYearsLoaded of FinancialYear list
    | FinancialCategoriesLoaded of FinancialCategory list
    | RemotingError of exn

type SettingsPageProps = 
    {| 
        CurrentUser: User
        CurrentBuildingId: BuildingId
    |}

let init (props: SettingsPageProps) =
    {
        CurrentUser = props.CurrentUser
        CurrentBuildingId = props.CurrentBuildingId
        IsLoadingFinancialCategories = true
        IsLoadingFinancialYears = true
        FinancialYears = []
        FinancialCategories = []
    },
    Cmd.batch [
        Cmd.OfAsync.either (Remoting.getRemotingApi()).GetFinancialCategories props.CurrentBuildingId FinancialCategoriesLoaded RemotingError
        Cmd.OfAsync.either (Remoting.getRemotingApi()).GetFinancialYears props.CurrentBuildingId FinancialYearsLoaded RemotingError
    ]

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | FinancialYearsLoaded financialYears ->
        { state with FinancialYears = financialYears; IsLoadingFinancialYears = false }, Cmd.none
    | FinancialCategoriesLoaded financialCategories ->
        { state with FinancialCategories = financialCategories; IsLoadingFinancialCategories = false }, Cmd.none
    | RemotingError error ->
        state, showGenericErrorModalCmd error


let view (state: State) (dispatch: Msg -> unit) = 
    [
        div [ Class Bootstrap.row ] [
            fieldset [ Class Bootstrap.col ] [
                legend [] [ h4 [] [ str "Financieel jaar" ] ]
                div [ Class Bootstrap.col7 ] [
                    str "Actieve Financiële jaren hier."
                ]
                div [ Class Bootstrap.col5 ] [
                    str "Geschiedenis"
                ]
            ]
        ]
        div [ Class Bootstrap.row ] [
            fieldset [ Class Bootstrap.col ] [
                legend [] [ h4 [] [ str "Rubrieken" ] ]
                let allCategories = state.FinancialCategories |> List.sortBy(fun fc -> fc.Code)
                let parents = allCategories |> List.filter(fun fc -> fc.Code.Length = 3 || fc.Code = "400000")
                let rec renderChildren parentCode level =
                    allCategories 
                    |> List.filter (fun fc -> fc.Code.StartsWith(parentCode) && fc.Code.Length = parentCode.Length + 1)
                    |> List.map (fun child -> 
                        [
                            li [] [ 
                                str (seq { for _ in 1..level do yield "" } |> String.JoinWith " ")
                                str (sprintf "%s - %s" child.Code child.Description) 
                            ]
                            ol [] [ yield! renderChildren child.Code (level + 1) ]
                        ] |> React.fragment)
            
                ol [] [
                    yield!
                        parents 
                        |> List.map (fun parent -> 
                            [
                                li [] [ str (sprintf "%s - %s" parent.Code parent.Description) ]
                                ol [] [ yield! renderChildren parent.Code 1 ]
                            ] |> React.fragment)
                ]
            ]
        ]
    ]
    |> React.fragment

let render (props: SettingsPageProps) =
    React.elmishComponent ("SettingsPage", init props, update, view)