module Client.Library

open Shared.Read

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