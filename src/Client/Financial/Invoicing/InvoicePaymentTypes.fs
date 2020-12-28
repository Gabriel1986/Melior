module Client.Financial.Invoicing.InvoicePaymentTypes

open System
open Shared.Read
open Shared.Write

type CreateOrUpdate =
    | Create
    | Update of InvoicePayment

type CreatedOrUpdated =
    | Created of InvoicePaymentInput
    | Updated of InvoicePaymentInput

module InvoicePaymentInput = 
    let Init (currentBuilding: BuildingListItem) (invoiceId: Guid) =
        let bankAccount = currentBuilding.BankAccounts |> List.tryHead
        {
            InvoiceId = invoiceId
            BuildingId = currentBuilding.BuildingId
            InvoicePaymentId = Guid.NewGuid()
            Amount = ""
            Date = DateTime.Today
            FromBankAccount = bankAccount
            FinancialCategoryId = bankAccount |> Option.bind (fun ba -> ba.FinancialCategoryId) 
            MediaFiles = []
        }
    let FromInvoicePayment (payment: InvoicePayment) = {
        InvoiceId = payment.InvoiceId
        BuildingId = payment.BuildingId
        InvoicePaymentId = payment.InvoicePaymentId
        Amount = String.Format("{0:0.00}", payment.Amount).Replace(".", ",")
        Date = payment.Date
        FromBankAccount = Some payment.FromBankAccount
        FinancialCategoryId = Some payment.FinancialCategory.FinancialCategoryId
        MediaFiles = payment.MediaFiles
    }