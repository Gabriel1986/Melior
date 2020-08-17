module Server.Media.MediaSystem

open Microsoft.Extensions.Configuration
open Server.AppSettings
open Server.Blueprint.Behavior.Media

let build (config: IConfiguration): IMediaSystem = 
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection
    let store = Storage.makeStorage conn
    {
        new IMediaSystem with
            member _.CreateMediaFile msg = Workflow.createMediaFile store msg
            member _.DeleteMediaFilesForEntity msg = Workflow.deleteMediaFilesForEntity store msg
            member _.DeleteMediaFile msg = Workflow.deleteMediaFile store msg
            member _.GetMediaFile partition fileId = Query.getMediaFileById conn partition fileId
            member _.GetMediaFilesForEntities partition entityIds = Query.getMediaFilesForEntities conn partition entityIds
            member me.HttpHandler = HttpHandler.mediaHandler config me
    }