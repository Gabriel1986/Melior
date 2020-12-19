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
            member _.CloseFinancialYear msg = Workflow.closeFinancialYear store conn msg
            member _.DeleteFinancialYear msg = Workflow.deleteFinancialYear store msg
            member _.GetFinancialYears msg = Query.getAllFinancialYears conn msg.Payload

            member _.CreateFinancialCategory msg = Workflow.createFinancialCategory store msg
            member _.UpdateFinancialCategory msg = Workflow.updateFinancialCategory store msg
            member _.DeleteFinancialCategory msg = Workflow.deleteFinancialCategory store msg
            member _.GetFinancialCategories msg = Query.getAllFinancialCategories conn msg.Payload
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
            | StorageEvent.LotEvent (LotEvent.LotOwnerWasAdded (validatedLot, validatedLotOwner)) ->                
                async {
                    match! Query.getNewFinancialCategoryCodeForLotOwner conn (validatedLot.BuildingId, validatedLotOwner) with
                    | Some financialCategoryCode ->
                        let newFinancialCategory: FinancialCategory = {
                            FinancialCategoryId = Guid.NewGuid()
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
                        match ValidatedFinancialCategory.Validate (newFinancialCategory) with
                        | Ok validated ->
                            return [ StorageEvent.FinancialEvent (FinancialEvent.FinancialCategoryEvent (BuildingSpecificCUDEvent.Created validated)) ]
                        | Error e ->
                            Serilog.Log.Logger.Error(sprintf "A validation error occured while trying to create a financial category for lotowner '%A': %A" validatedLotOwner.LotOwnerId e)
                            return []
                    | None ->
                        return []
                }
            | StorageEvent.LotEvent (LotEvent.LotOwnerWasDeleted (buildingId: BuildingId, lotOwnerId: Guid)) ->
                async {
                    match! Query.getFinancialCategoryByLotOwnerId conn (buildingId, lotOwnerId) with
                    | Some financialCategory ->
                        return [ StorageEvent.FinancialEvent (FinancialEvent.FinancialCategoryEvent (BuildingSpecificCUDEvent.Deleted (buildingId, financialCategory.FinancialCategoryId))) ]
                    | None ->
                        return []
                }
            | _ -> 
                Async.lift []