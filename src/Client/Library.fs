module Client.Library

open Shared.Read
open Elmish.SweetAlert
open Browser.Dom

let getCachedCurrentBuilding (currentUser: User) =
    let currentBuildingStr = Browser.WebStorage.localStorage.getItem("currentBuilding")
    if currentBuildingStr = null || currentBuildingStr.Length < 0 
    then None
    else 
        let currentBuildingOpt = Thoth.Json.Decode.Auto.fromString<BuildingListItem>(currentBuildingStr)
        match currentBuildingOpt with
        | Ok currentBuilding -> 
            if currentUser.HasAccessToBuilding (currentBuilding.BuildingId) then
                Some currentBuilding
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