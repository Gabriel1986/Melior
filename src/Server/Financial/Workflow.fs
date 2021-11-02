module Server.Financial.Workflow

open System
open Shared.Read
open Shared.Write
open Shared.Remoting
open Server.Library
open Server.LibraryExtensions
open Server.Blueprint.Behavior.Storage
open Server.Blueprint.Data.Storage

let (|Authorized|Unauthorized|) (currentUser: User, buildingId: BuildingId option) =
    match buildingId with
    | Some buildingId when currentUser.HasAccessToBuilding buildingId -> Authorized
    | None when currentUser.IsSysAdmin() -> Authorized
    | _ -> Unauthorized

let createDistributionKey (store: IStorageEngine) (msg: Message<DistributionKey>) = async {
    match (msg.CurrentUser, msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedDistributionKey.Validate msg.Payload with
        | Ok validated -> 
            let! _ = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Created
                |> FinancialEvent.DistributionKeyEvent
                |> StorageEvent.FinancialEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveDistributionKeyError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveDistributionKeyError.AuthorizationError
}

let updateDistributionKey (store: IStorageEngine) (msg: Message<DistributionKey>) = async {
    match (msg.CurrentUser, msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedDistributionKey.Validate msg.Payload with
        | Ok validated ->
            let! nbUpdated = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Updated
                |> FinancialEvent.DistributionKeyEvent
                |> StorageEvent.FinancialEvent
                |> inMsg msg
            ]
            return if nbUpdated > 0 then Ok () else Error (SaveDistributionKeyError.NotFound)
        | Error validationErrors ->
            return Error (SaveDistributionKeyError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveDistributionKeyError.AuthorizationError
}

let deleteDistributionKey (store: IStorageEngine) (msg: Message<BuildingId * Guid>) = async {
    match (msg.CurrentUser, Some (fst msg.Payload)) with
    | Authorized ->
        let! nbRows = store.PersistTransactional [
            msg.Payload
            |> BuildingSpecificCUDEvent.Deleted
            |> FinancialEvent.DistributionKeyEvent
            |> StorageEvent.FinancialEvent
            |> inMsg msg
        ]
        return if nbRows > 0 then Ok () else Error DeleteDistributionKeyError.NotFound
    | Unauthorized ->
        return Error DeleteDistributionKeyError.AuthorizationError
}

let seedDistributionKeys (store: IStorageEngine) (msg: Message<DistributionKey list>) =
    store.PersistTransactional [
        yield!
            msg.Payload
            |> List.map (fun dKey ->
                match dKey |> ValidatedDistributionKey.Validate with
                | Ok validated ->
                    validated
                    |> BuildingSpecificCUDEvent.Created
                    |> FinancialEvent.DistributionKeyEvent
                    |> StorageEvent.FinancialEvent
                    |> inMsg msg
                | Error e ->
                    failwithf "Precondition failed: An error occured while seeding distribution keys: %A" e
            )
    ]
    |> Async.Ignore

let createInvoice (store: IStorageEngine) (msg: Message<Invoice>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedInvoice.Validate msg.Payload with
        | Ok validated -> 
            let! _ = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Created
                |> FinancialEvent.InvoiceEvent
                |> StorageEvent.FinancialEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveInvoiceError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveInvoiceError.AuthorizationError
}

let updateInvoice (store: IStorageEngine) (msg: Message<Invoice>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedInvoice.Validate msg.Payload with
        | Ok validated ->
            let! nbUpdated = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Updated
                |> FinancialEvent.InvoiceEvent
                |> StorageEvent.FinancialEvent
                |> inMsg msg
            ]
            return if nbUpdated > 0 then Ok () else Error (SaveInvoiceError.NotFound)
        | Error validationErrors ->
            return Error (SaveInvoiceError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveInvoiceError.AuthorizationError
}

let deleteInvoice (store: IStorageEngine) (msg: Message<BuildingId * Guid>) = async {
    match (msg.CurrentUser, Some (fst msg.Payload)) with
    | Authorized ->
        let! nbRows = store.PersistTransactional [
            msg.Payload
            |> BuildingSpecificCUDEvent.Deleted
            |> FinancialEvent.InvoiceEvent
            |> StorageEvent.FinancialEvent
            |> inMsg msg
        ]
        return if nbRows > 0 then Ok () else Error DeleteInvoiceError.NotFound
    | Unauthorized ->
        return Error DeleteInvoiceError.AuthorizationError
}

let createInvoicePayment (store: IStorageEngine) (msg: Message<InvoicePayment>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedInvoicePayment.Validate msg.Payload with
        | Ok validated ->
            let! _ = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Created
                |> FinancialEvent.InvoicePaymentEvent
                |> StorageEvent.FinancialEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveInvoicePaymentError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveInvoicePaymentError.AuthorizationError
}

let updateInvoicePayment (store: IStorageEngine) (msg: Message<InvoicePayment>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedInvoicePayment.Validate msg.Payload with
        | Ok validated ->
            let! nbUpdated = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Updated
                |> FinancialEvent.InvoicePaymentEvent
                |> StorageEvent.FinancialEvent
                |> inMsg msg
            ]
            return if nbUpdated > 0 then Ok () else Error (SaveInvoicePaymentError.NotFound)
        | Error validationErrors ->
            return Error (SaveInvoicePaymentError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveInvoicePaymentError.AuthorizationError
}

let deleteInvoicePayment (store: IStorageEngine) (msg: Message<BuildingId * Guid>) = async {
    match (msg.CurrentUser, Some (fst msg.Payload)) with
    | Authorized ->
        let! nbRows = store.PersistTransactional [
            msg.Payload
            |> BuildingSpecificCUDEvent.Deleted
            |> FinancialEvent.InvoicePaymentEvent
            |> StorageEvent.FinancialEvent
            |> inMsg msg
        ]
        return if nbRows > 0 then Ok () else Error DeleteInvoicePaymentError.NotFound
    | Unauthorized ->
        return Error DeleteInvoicePaymentError.AuthorizationError
}

let createFinancialYear (store: IStorageEngine) (msg: Message<FinancialYear>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedFinancialYear.Validate msg.Payload with
        | Ok validated ->
            let! _ = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Created
                |> FinancialEvent.FinancialYearEvent
                |> StorageEvent.FinancialEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveFinancialYearError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveFinancialYearError.AuthorizationError
}

let updateFinancialYear (store: IStorageEngine) (msg: Message<FinancialYear>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedFinancialYear.Validate msg.Payload with
        | Ok validated ->
            let! nbUpdated = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Updated
                |> FinancialEvent.FinancialYearEvent
                |> StorageEvent.FinancialEvent
                |> inMsg msg
            ]
            return if nbUpdated > 0 then Ok () else Error (SaveFinancialYearError.NotFound)
        | Error validationErrors ->
            return Error (SaveFinancialYearError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveFinancialYearError.AuthorizationError
}

let closeFinancialYear (store: IStorageEngine) (getFinancialYearsByIds: BuildingId -> Guid list -> Async<FinancialYear list>) (msg: Message<BuildingId * Guid>) = async {
    match (msg.CurrentUser, Some (fst msg.Payload)) with
    | Authorized ->
        let! financialYears = getFinancialYearsByIds (fst msg.Payload) [ snd msg.Payload ]
        match financialYears |> List.map ValidatedFinancialYear.Validate with
        | [] -> 
            return Error SaveFinancialYearError.NotFound
        | (Ok validated)::_ ->
            let! nbRows = store.PersistTransactional [
                validated
                |> FinancialEvent.FinancialYearWasClosed
                |> StorageEvent.FinancialEvent
                |> inMsg msg
            ]
            return if nbRows > 0 then Ok () else Error SaveFinancialYearError.NotFound
        | (Error e)::_ ->
            return failwithf "Precondition failed, could not validate an already stored financial year: %A" e
    | Unauthorized ->
        return Error SaveFinancialYearError.AuthorizationError
}

let deleteFinancialYear (store: IStorageEngine) (msg: Message<BuildingId * Guid>) = async {
    match (msg.CurrentUser, Some (fst msg.Payload)) with
    | Authorized ->
        let! nbRows = store.PersistTransactional [
            msg.Payload
            |> BuildingSpecificCUDEvent.Deleted
            |> FinancialEvent.FinancialYearEvent
            |> StorageEvent.FinancialEvent
            |> inMsg msg
        ]
        return if nbRows > 0 then Ok () else Error DeleteFinancialYearError.NotFound
    | Unauthorized ->
        return Error DeleteFinancialYearError.AuthorizationError
}

let createFinancialCategory (store: IStorageEngine) (msg: Message<FinancialCategory>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedFinancialCategory.Validate msg.Payload with
        | Ok validated -> 
            let! _ = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Created
                |> FinancialEvent.FinancialCategoryEvent
                |> StorageEvent.FinancialEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveFinancialCategoryError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveFinancialCategoryError.AuthorizationError
}

let updateFinancialCategory (store: IStorageEngine) (msg: Message<FinancialCategory>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedFinancialCategory.Validate msg.Payload with
        | Ok validated ->
            let! nbUpdated = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Updated
                |> FinancialEvent.FinancialCategoryEvent
                |> StorageEvent.FinancialEvent
                |> inMsg msg            
            ]
            return if nbUpdated > 0 then Ok () else Error (SaveFinancialCategoryError.NotFound)
        | Error validationErrors ->
            return Error (SaveFinancialCategoryError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveFinancialCategoryError.AuthorizationError
}

let deleteFinancialCategory (store: IStorageEngine) (msg: Message<BuildingId * Guid>) = async {
    match (msg.CurrentUser, Some (fst msg.Payload)) with
    | Authorized ->
        let! nbRows = store.PersistTransactional [
            msg.Payload
            |> BuildingSpecificCUDEvent.Deleted
            |> FinancialEvent.FinancialCategoryEvent
            |> StorageEvent.FinancialEvent
            |> inMsg msg
        ]
        return if nbRows > 0 then Ok () else Error DeleteFinancialCategoryError.NotFound
    | Unauthorized ->
        return Error DeleteFinancialCategoryError.AuthorizationError
}

let seedFinancialCategories (msg: Message<FinancialCategory list>) =
    [
        yield!
            msg.Payload
            |> List.map (fun cat ->
                match cat |> ValidatedFinancialCategory.Validate with
                | Ok validated ->
                    validated
                    |> BuildingSpecificCUDEvent.Created
                    |> FinancialEvent.FinancialCategoryEvent
                    |> StorageEvent.FinancialEvent
                | Error e ->
                    failwithf "Precondition failed: An error occured while seeding financial categories: %A" e
            )
    ]

//let createFinancialTransaction (store: IStorageEngine) (msg: Message<FinancialTransaction>) = async {
//    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
//    | Authorized ->
//        match ValidatedFinancialTransaction.Validate msg.Payload with
//        | Ok validated -> 
//            let! _ = store.PersistTransactional [
//                validated
//                |> BuildingSpecificCUDEvent.Created
//                |> FinancialEvent.FinancialTransactionEvent
//                |> StorageEvent.FinancialEvent
//                |> inMsg msg
//            ]
//            return Ok ()
//        | Error validationErrors ->
//            return Error (SaveFinancialTransactionError.Validation validationErrors)
//    | Unauthorized ->
//        return Error SaveFinancialTransactionError.AuthorizationError
//}

//let updateFinancialTransaction (store: IStorageEngine) (msg: Message<FinancialTransaction>) = async {
//    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
//    | Authorized ->
//        match ValidatedFinancialTransaction.Validate msg.Payload with
//        | Ok validated ->
//            let! nbUpdated = store.PersistTransactional [
//                validated
//                |> BuildingSpecificCUDEvent.Updated
//                |> FinancialEvent.FinancialTransactionEvent
//                |> StorageEvent.FinancialEvent
//                |> inMsg msg
//            ]
//            return if nbUpdated > 0 then Ok () else Error (SaveFinancialTransactionError.NotFound)
//        | Error validationErrors ->
//            return Error (SaveFinancialTransactionError.Validation validationErrors)
//    | Unauthorized ->
//        return Error SaveFinancialTransactionError.AuthorizationError
//}

//let deleteFinancialTransaction (store: IStorageEngine) (msg: Message<BuildingId * Guid>) = async {
//    match (msg.CurrentUser, Some (fst msg.Payload)) with
//    | Authorized ->
//        let! nbRows = store.PersistTransactional [
//            msg.Payload
//            |> BuildingSpecificCUDEvent.Deleted
//            |> FinancialEvent.FinancialTransactionEvent
//            |> StorageEvent.FinancialEvent
//            |> inMsg msg
//        ]
//        return if nbRows > 0 then Ok () else Error DeleteFinancialTransactionError.NotFound
//    | Unauthorized ->
//        return Error DeleteFinancialTransactionError.AuthorizationError
//}

let createDepositRequest (store: IStorageEngine) (msg: Message<DepositRequest>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedDepositRequest.Validate msg.Payload with
        | Ok validated ->
            let! _ = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Created
                |> FinancialEvent.DepositRequestEvent
                |> StorageEvent.FinancialEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveDepositRequestError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveDepositRequestError.AuthorizationError
}

let updateDepositRequest (store: IStorageEngine) (msg: Message<DepositRequest>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedDepositRequest.Validate msg.Payload with
        | Ok validated ->
            let! nbUpdated = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Updated
                |> FinancialEvent.DepositRequestEvent
                |> StorageEvent.FinancialEvent
                |> inMsg msg
            ]
            return if nbUpdated > 0 then Ok () else Error (SaveDepositRequestError.NotFound)
        | Error validationErrors ->
            return Error (SaveDepositRequestError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveDepositRequestError.AuthorizationError
}

let deleteDepositRequest (store: IStorageEngine) (msg: Message<BuildingId * Guid>) = async {
    match (msg.CurrentUser, Some (fst msg.Payload)) with
    | Authorized ->
        let! nbRows = store.PersistTransactional [
            msg.Payload
            |> BuildingSpecificCUDEvent.Deleted
            |> FinancialEvent.DepositRequestEvent
            |> StorageEvent.FinancialEvent
            |> inMsg msg
        ]
        return if nbRows > 0 then Ok () else Error DeleteDepositRequestError.NotFound
    | Unauthorized ->
        return Error DeleteDepositRequestError.AuthorizationError
}

let createDeposit (store: IStorageEngine) (msg: Message<Deposit>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedDeposit.Validate msg.Payload with
        | Ok validated ->
            let! _ = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Created
                |> FinancialEvent.DepositEvent
                |> StorageEvent.FinancialEvent
                |> inMsg msg
            ]
            return Ok ()
        | Error validationErrors ->
            return Error (SaveDepositError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveDepositError.AuthorizationError
}

let updateDeposit (store: IStorageEngine) (msg: Message<Deposit>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedDeposit.Validate msg.Payload with
        | Ok validated ->
            let! nbUpdated = store.PersistTransactional [
                validated
                |> BuildingSpecificCUDEvent.Updated
                |> FinancialEvent.DepositEvent
                |> StorageEvent.FinancialEvent
                |> inMsg msg
            ]
            return if nbUpdated > 0 then Ok () else Error (SaveDepositError.NotFound)
        | Error validationErrors ->
            return Error (SaveDepositError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveDepositError.AuthorizationError
}

let deleteDeposit (store: IStorageEngine) (msg: Message<BuildingId * Guid>) = async {
    match (msg.CurrentUser, Some (fst msg.Payload)) with
    | Authorized ->
        let! nbRows = store.PersistTransactional [
            msg.Payload
            |> BuildingSpecificCUDEvent.Deleted
            |> FinancialEvent.DepositEvent
            |> StorageEvent.FinancialEvent
            |> inMsg msg
        ]
        return if nbRows > 0 then Ok () else Error DeleteDepositError.NotFound
    | Unauthorized ->
        return Error DeleteDepositError.AuthorizationError
}