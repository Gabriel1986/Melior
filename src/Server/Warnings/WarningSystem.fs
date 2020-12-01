module Server.Warnings.WarningSystem

open Microsoft.Extensions.Configuration

open Server.AppSettings
open Server.Blueprint.Behavior.Warnings

let build (config: IConfiguration): IWarningSystem = 
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection
    {
        new IWarningSystem with
            member _.GetWarnings buildingId = Query.getWarnings conn buildingId
    }