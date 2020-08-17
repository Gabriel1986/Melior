module Client.Owners.OwnerViewComponent

open Fable.React
open Shared.Read
open Client.Components
open Client.ClientStyle.Helpers

let view (building: BuildingListItem) (owner: Owner) =
    div [] [
        readonlyFormElement (sprintf "Bewoner van %s?" building.Code) (if owner.IsResident then "Ja" else "Nee")
        PersonViewComponent.render {| Person = owner.Person; WithAddresses = owner.Person.MainAddress <> building.Address |}
    ]

let render =
    FunctionComponent.Of ((fun (props: {| Building: BuildingListItem; Owner: Owner |}) -> view props.Building props.Owner), memoizeWith = memoEqualsButFunctions)