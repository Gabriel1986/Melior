module Client.Library

open Shared.Read
open Elmish.SweetAlert
open Browser.Dom

let getCurrentBuilding (currentUser: CurrentUser) =
    let currentBuildingStr = Browser.WebStorage.localStorage.getItem("currentBuilding")
    if currentBuildingStr = null || currentBuildingStr.Length < 0 
    then None
    else 
        let r = Thoth.Json.Decode.Auto.fromString<BuildingListItem>(currentBuildingStr)
        match r with
        | Ok r -> 
            if currentUser.BuildingIds |> List.contains r.BuildingId then
                Some r
            else
                None
        | Error e -> 
            printf "%A" e
            None

let showErrorToastCmd (message: string) =
    let alert = 
        (ToastAlert(message))
            .Timeout(3000)
            .ConfirmButton(false)
            .Position(AlertPosition.TopEnd)
            .Type(AlertType.Error)
    SweetAlert.Run(alert)

let showGenericErrorModalCmd (e: exn) =
    console.error(string e)
    let modal =
        (SimpleAlert("Er is iets misgelopen bij de communicatie met de server, gelieve de pagina te verversen als dit probleem zich blijft voordoen."))
            .Timeout(3000)
            .Type(AlertType.Error)
    SweetAlert.Run(modal)