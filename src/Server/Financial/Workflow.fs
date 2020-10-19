module Server.Financial.Workflow

open System
open Storage
open Server.Library
open Server.LibraryExtensions
open Shared.Read
open Shared.Write
open Shared.Remoting

let (|Authorized|Unauthorized|) (currentUser: User, buildingId: BuildingId option) =
    match buildingId with
    | Some buildingId when currentUser.HasAccessToBuilding buildingId -> Authorized
    | None when currentUser.IsSysAdmin() -> Authorized
    | _ -> Unauthorized

let createDistributionKey (store: IFinancialStorage) (msg: Message<DistributionKey>) = async {
    match (msg.CurrentUser, msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedDistributionKey.Validate (msg.Payload) with
        | Ok validated -> 
            do! store.CreateDistributionKey (msg |> Message.map validated)
            return Ok ()
        | Error validationErrors ->
            return Error (SaveDistributionKeyError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveDistributionKeyError.AuthorizationError
}

let updateDistributionKey (store: IFinancialStorage) (msg: Message<DistributionKey>) = async {
    match (msg.CurrentUser, msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedDistributionKey.Validate (msg.Payload) with
        | Ok validated ->
            let! nbUpdated = store.UpdateDistributionKey (msg |> Message.map validated)
            return if nbUpdated > 0 then Ok () else Error (SaveDistributionKeyError.NotFound)
        | Error validationErrors ->
            return Error (SaveDistributionKeyError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveDistributionKeyError.AuthorizationError
}

let deleteDistributionKey (store: IFinancialStorage) (msg: Message<BuildingId * Guid>) = async {
    match (msg.CurrentUser, Some (fst msg.Payload)) with
    | Authorized ->
        let! nbRows = store.DeleteDistributionKey msg
        return if nbRows > 0 then Ok () else Error DeleteDistributionKeyError.NotFound
    | Unauthorized ->
        return Error DeleteDistributionKeyError.AuthorizationError
}

let createInvoice (store: IFinancialStorage) (msg: Message<Invoice>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedInvoice.Validate (msg.Payload) with
        | Ok validated -> 
            do! store.CreateInvoice (msg |> Message.map validated)
            return Ok ()
        | Error validationErrors ->
            return Error (SaveInvoiceError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveInvoiceError.AuthorizationError
}

let updateInvoice (store: IFinancialStorage) (msg: Message<Invoice>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedInvoice.Validate (msg.Payload) with
        | Ok validated ->
            let! nbUpdated = store.UpdateInvoice (msg |> Message.map validated)
            return if nbUpdated > 0 then Ok () else Error (SaveInvoiceError.NotFound)
        | Error validationErrors ->
            return Error (SaveInvoiceError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveInvoiceError.AuthorizationError
}

let deleteInvoice (store: IFinancialStorage) (msg: Message<BuildingId * Guid>) = async {
    match (msg.CurrentUser, Some (fst msg.Payload)) with
    | Authorized ->
        let! nbRows = store.DeleteInvoice msg
        return if nbRows > 0 then Ok () else Error DeleteInvoiceError.NotFound
    | Unauthorized ->
        return Error DeleteInvoiceError.AuthorizationError
}

let createFinancialYear (store: IFinancialStorage) (msg: Message<FinancialYear>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedFinancialYear.Validate (msg.Payload) with
        | Ok validated -> 
            do! store.CreateFinancialYear (msg |> Message.map validated)
            return Ok ()
        | Error validationErrors ->
            return Error (SaveFinancialYearError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveFinancialYearError.AuthorizationError
}

let updateFinancialYear (store: IFinancialStorage) (msg: Message<FinancialYear>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedFinancialYear.Validate (msg.Payload) with
        | Ok validated ->
            let! nbUpdated = store.UpdateFinancialYear (msg |> Message.map validated)
            return if nbUpdated > 0 then Ok () else Error (SaveFinancialYearError.NotFound)
        | Error validationErrors ->
            return Error (SaveFinancialYearError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveFinancialYearError.AuthorizationError
}

let deleteFinancialYear (store: IFinancialStorage) (msg: Message<BuildingId * Guid>) = async {
    match (msg.CurrentUser, Some (fst msg.Payload)) with
    | Authorized ->
        let! nbRows = store.DeleteFinancialYear msg
        return if nbRows > 0 then Ok () else Error DeleteFinancialYearError.NotFound
    | Unauthorized ->
        return Error DeleteFinancialYearError.AuthorizationError
}

let createFinancialCategory (store: IFinancialStorage) (msg: Message<FinancialCategory>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedFinancialCategory.Validate (msg.Payload) with
        | Ok validated -> 
            do! store.CreateFinancialCategory (msg |> Message.map validated)
            return Ok ()
        | Error validationErrors ->
            return Error (SaveFinancialCategoryError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveFinancialCategoryError.AuthorizationError
}

let updateFinancialCategory (store: IFinancialStorage) (msg: Message<FinancialCategory>) = async {
    match (msg.CurrentUser, Some msg.Payload.BuildingId) with
    | Authorized ->
        match ValidatedFinancialCategory.Validate (msg.Payload) with
        | Ok validated ->
            let! nbUpdated = store.UpdateFinancialCategory (msg |> Message.map validated)
            return if nbUpdated > 0 then Ok () else Error (SaveFinancialCategoryError.NotFound)
        | Error validationErrors ->
            return Error (SaveFinancialCategoryError.Validation validationErrors)
    | Unauthorized ->
        return Error SaveFinancialCategoryError.AuthorizationError
}

let deleteFinancialCategory (store: IFinancialStorage) (msg: Message<BuildingId * Guid>) = async {
    match (msg.CurrentUser, Some (fst msg.Payload)) with
    | Authorized ->
        let! nbRows = store.DeleteFinancialCategory msg
        return if nbRows > 0 then Ok () else Error DeleteFinancialCategoryError.NotFound
    | Unauthorized ->
        return Error DeleteFinancialCategoryError.AuthorizationError
}