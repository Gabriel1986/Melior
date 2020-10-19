module Server.Financial.FinancialSystem

open Microsoft.Extensions.Configuration
open Server.Library
open Server.LibraryExtensions
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
            member _.GetInvoices msg =
                if msg.CurrentUser.HasAccessToBuilding msg.Payload.BuildingId
                then Query.getInvoices conn msg.Payload
                else Async.lift []
            member _.GetInvoice msg = async {
                match! Query.getInvoice conn msg.Payload with
                | Some invoice when msg.CurrentUser.HasAccessToBuilding invoice.BuildingId ->
                    return Some invoice
                | _ ->
                    return None
            }

            member _.CreateFinancialYear msg = Workflow.createFinancialYear store msg
            member _.UpdateFinancialYear msg = Workflow.updateFinancialYear store msg
            member _.DeleteFinancialYear msg = Workflow.deleteFinancialYear store msg
            member _.GetFinancialYears msg = Query.getAllFinancialYears conn msg.Payload

            member _.CreateFinancialCategory msg = Workflow.createFinancialCategory store msg
            member _.UpdateFinancialCategory msg = Workflow.updateFinancialCategory store msg
            member _.DeleteFinancialCategory msg = Workflow.deleteFinancialCategory store msg
            member _.GetFinancialCategoryTree msg = Query.getAllFinancialCategories conn msg.Payload
    }