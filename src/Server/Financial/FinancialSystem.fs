module Server.Financial.FinancialSystem

open System
open Microsoft.Extensions.Configuration
open Shared.Read
open Shared.Write
open Server.Library
open Server.LibraryExtensions
open Server.AppSettings
open Server.Blueprint.Behavior.Financial
open Server.Blueprint.Behavior.Storage
open Server.Blueprint.Data.Storage
open Server.SeedData

let createMessage payload = {
    Payload = payload
    Context = None
    CreatedAt = DateTimeOffset.Now
}

let build (config: IConfiguration) (store: IStorageEngine): IFinancialSystem = 
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection
    {
        new IFinancialSystem with
            member _.CreateDistributionKey msg = Workflow.createDistributionKey store msg
            member _.UpdateDistributionKey msg = Workflow.updateDistributionKey store msg
            member _.DeleteDistributionKey msg = Workflow.deleteDistributionKey store msg
            member _.GetDistributionKeys msg = Query.getDistributionKeys conn msg.Payload
            member _.GetDistributionKey msg = async {
                match! Query.getDistributionKey conn msg.Payload with
                | Some distributionKey when (distributionKey.BuildingId.IsNone || msg.CurrentUser.HasAccessToBuilding distributionKey.BuildingId.Value) ->
                    return Some distributionKey
                | _ ->
                    return None
            }
            member _.GetDistributionKeyListItems msg = Query.getDistributionKeyListItems conn msg.Payload
            member _.SeedDistributionKeys keys = Workflow.seedDistributionKeys store (keys |> createMessage)

            member _.CreateInvoice msg = Workflow.createInvoice store msg
            member _.UpdateInvoice msg = Workflow.updateInvoice store msg
            member _.DeleteInvoice msg = Workflow.deleteInvoice store msg
            member _.GetInvoices msg =
                match msg.CurrentUser.HasAccessToBuilding msg.Payload.BuildingId with
                | true  -> Query.getInvoices conn msg.Payload
                | false -> Async.lift []
            member _.GetInvoice msg = async {
                match! Query.getInvoice conn msg.Payload with
                | Some invoice when msg.CurrentUser.HasAccessToBuilding invoice.BuildingId ->
                    return Some invoice
                | _ ->
                    return None
            }

            member _.CreateDepositRequest msg = Workflow.createDepositRequest store msg
            member _.UpdateDepositRequest msg = Workflow.updateDepositRequest store msg
            member _.DeleteDepositRequest msg = Workflow.deleteDepositRequest store msg
            member _.GetDepositRequests msg =
                match msg.CurrentUser.HasAccessToBuilding msg.Payload.BuildingId with
                | true -> Query.getDepositRequests conn msg.Payload
                | false -> Async.lift []

            member _.GetDepositRequest msg = async {
                match! Query.getDepositRequest conn msg.Payload with
                | Some request when msg.CurrentUser.HasAccessToBuilding request.BuildingId ->
                    return Some request
                | _ ->
                    return None
            }

            member _.CreateInvoicePayment msg = Workflow.createInvoicePayment store msg
            member _.UpdateInvoicePayment msg = Workflow.updateInvoicePayment store msg
            member _.DeleteInvoicePayment msg = Workflow.deleteInvoicePayment store msg

            member _.CreateDeposit msg = Workflow.createDeposit store msg
            member _.UpdateDeposit msg = Workflow.updateDeposit store msg
            member _.DeleteDeposit msg = Workflow.deleteDeposit store msg

            member _.CreateFinancialYear msg = Workflow.createFinancialYear store msg
            member _.UpdateFinancialYear msg = Workflow.updateFinancialYear store msg
            member _.CloseFinancialYear msg = Workflow.closeFinancialYear store (Query.getFinancialYearsByIds conn) msg
            member _.DeleteFinancialYear msg = Workflow.deleteFinancialYear store msg
            member _.GetFinancialYears msg = Query.getAllFinancialYears conn msg.Payload

            member _.CreateFinancialCategory msg = Workflow.createFinancialCategory store msg
            member _.UpdateFinancialCategory msg = Workflow.updateFinancialCategory store msg
            member _.DeleteFinancialCategory msg = Workflow.deleteFinancialCategory store msg
            member _.GetFinancialCategories msg = Query.getAllFinancialCategories conn msg.Payload

            //member _.CreateFinancialTransaction msg = Workflow.createFinancialTransaction store msg
            //member _.UpdateFinancialTransaction msg = Workflow.updateFinancialTransaction store msg
            //member _.DeleteFinancialTransaction msg = Workflow.deleteFinancialTransaction store msg
            member _.GetFinancialTransactions msg =
                match msg.CurrentUser.HasAccessToBuilding msg.Payload.BuildingId with
                | true -> Query.getFinancialTransactions conn msg.Payload
                | false -> Async.lift []
    }

type ReactiveBehavior (config: IConfiguration) =
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection

    interface IReactiveBehavior with
        override _.ReactTo message =
            match message.Payload with
            | StorageEvent.BuildingEvent (BuildingEvent.BuildingEvent (CUDEvent.Created validated)) -> 
                async {
                    let! categories = FinancialCategories.readPredefined (validated.BuildingId)
                    return Workflow.seedFinancialCategories (categories |> inMsg message)
                }
            | StorageEvent.LotEvent (LotEvent.LotOwnerEvent (BuildingSpecificCUDEvent.Created (validatedLot, validatedLotOwner))) ->
                async {
                    let! financialCategoryCodes = Query.getNewFinancialCategoryCodesForLotOwner conn (validatedLot.BuildingId, validatedLotOwner)
                    return
                        financialCategoryCodes
                        |> List.choose (fun financialCategoryCode -> 
                            {
                                FinancialCategory.FinancialCategoryId = Guid.NewGuid()
                                BuildingId = validatedLot.BuildingId
                                Code = financialCategoryCode
                                Description =
                                    let name =
                                        match validatedLotOwner.LotOwnerType with
                                        | LotOwnerType.Organization org -> org.Name
                                        | LotOwnerType.Owner owner -> owner.FullName ()
                                    let lotName = string validatedLot.Code
                                    sprintf "%s - %s" name lotName
                                LotOwnerId = Some validatedLotOwner.LotOwnerId
                            }
                            |> ValidatedFinancialCategory.Validate
                            |> (function 
                                | Ok validated -> Some validated
                                | Error e ->
                                    Serilog.Log.Logger.Error(sprintf "A validation error occured while trying to create a financial category for lotowner '%A': %A" validatedLotOwner.LotOwnerId e)
                                    None
                            ))
                        |> List.map (BuildingSpecificCUDEvent.Created >> FinancialEvent.FinancialCategoryEvent >> StorageEvent.FinancialEvent)
                }
            | StorageEvent.LotEvent (LotEvent.LotOwnerEvent (BuildingSpecificCUDEvent.Deleted (buildingId: BuildingId, lotOwnerId: Guid))) ->
                async {
                    match! Query.getFinancialCategoryByLotOwnerId conn (buildingId, lotOwnerId) with
                    | Some financialCategory ->
                        return [ StorageEvent.FinancialEvent (FinancialEvent.FinancialCategoryEvent (BuildingSpecificCUDEvent.Deleted (buildingId, financialCategory.FinancialCategoryId))) ]
                    | None ->
                        return []
                }
            | _ -> 
                Async.lift []