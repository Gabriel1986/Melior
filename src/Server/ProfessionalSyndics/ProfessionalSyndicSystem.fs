module Server.ProfessionalSyndics.ProfessionalSyndicSystem

open Microsoft.Extensions.Configuration
open Server.AppSettings
open Server.Blueprint.Behavior.ProfessionalSyndics

let build (config: IConfiguration): IProfessionalSyndicSystem =
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.ConnectionString
    let store = Storage.makeStorage conn
    {
        new IProfessionalSyndicSystem with
            member _.CreateProfessionalSyndic msg = Workflow.createProfessionalSyndic store msg
            member _.UpdateProfessionalSyndic msg = Workflow.updateProfessionalSyndic store msg
            member _.DeleteProfessionalSyndic msg = Workflow.deleteProfessionalSyndic store msg
            member _.GetProfessionalSyndic msg = Query.getProfessionalSyndic conn msg.Payload
            member _.GetProfessionalSyndics msg = Query.getProfessionalSyndics conn msg.Payload
    }