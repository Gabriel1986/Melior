module Server.Financial.FinancialSystem

open Microsoft.Extensions.Configuration
open Server.Library
open Server.AppSettings
open Server.Blueprint.Behavior.Financial

let build (config: IConfiguration): IFinancialSystem = 
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection
    let store = Storage.makeStorage conn
    {
        new IFinancialSystem with
            member _.CreateDistributionKey msg = Workflow.createDistributionKey store msg
            member _.UpdateDistributionKey msg = Workflow.updateDistributionKey store msg
            member _.DeleteDistributionKey msg = Workflow.deleteDistributionKey store msg
            member _.GetDistributionKeys msg = Query.getDistributionKeys conn msg.Payload
            member _.GetDistributionKeyListItems msg = Query.getDistributionKeyListItems conn msg.Payload

            member _.CreateInvoice msg = Workflow.createInvoice store msg
            member _.UpdateInvoice msg = Workflow.updateInvoice store msg
            member _.DeleteInvoice msg = Workflow.deleteInvoice store msg
            member _.GetInvoices msg = Query.getInvoices conn msg.Payload
            member _.GetInvoice msg = Query.getInvoice conn msg.Payload

            member _.GetFinancialYears msg = Query.getFinancialYears conn msg.Payload
            member _.GetFinancialCategories msg = Query.getFinancialCategories conn msg.Payload
    }