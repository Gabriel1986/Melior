module Client.Financial.Invoicing.InvoicePaymentTypes

open System
open Shared.Trial
open Shared.Trial.Control
open Shared.Read
open Shared.MediaLibrary

type CreateOrUpdate =
    | Create
    | Update of InvoicePayment

type CreatedOrUpdated =
    | Created of InvoicePayment
    | Updated of InvoicePayment

type InvoicePaymentForm = 
    {
        InvoiceId: Guid
        BuildingId: Guid
        InvoicePaymentId: Guid
        Amount: string
        Date: DateTime
        FromBankAccount: BankAccount option
        FinancialCategoryId: Guid option
        MediaFiles: MediaFile list
    }
    static member Init (currentBuilding: BuildingListItem) (invoiceId: Guid) =
        let bankAccount = currentBuilding.BankAccounts |> List.tryHead
        {
            InvoicePaymentForm.InvoiceId = invoiceId
            BuildingId = currentBuilding.BuildingId
            InvoicePaymentId = Guid.NewGuid()
            Amount = ""
            Date = DateTime.Today
            FromBankAccount = bankAccount
            FinancialCategoryId = bankAccount |> Option.bind (fun ba -> ba.FinancialCategoryId) 
            MediaFiles = []
        }
    static member FromInvoicePayment (payment: InvoicePayment) = {
        InvoicePaymentForm.InvoiceId = payment.InvoiceId
        BuildingId = payment.BuildingId
        InvoicePaymentId = payment.InvoicePaymentId
        Amount = String.Format("{0:0.00}", payment.Amount).Replace(".", ",")
        Date = payment.Date
        FromBankAccount = Some payment.FromBankAccount
        FinancialCategoryId = Some payment.FinancialCategoryId
        MediaFiles = payment.MediaFiles
    }
    member me.ToInvoicePayment () =
        let validateDecimal (path: string) (decimalString: string) =
            match decimalString with
            | empty when String.IsNullOrWhiteSpace(empty) -> Trial.ofError (path, "Verplicht veld")
            | other ->
                match Decimal.TryParse(other.Replace(',', '.')) with
                | true, parsed -> Trial.Pass parsed
                | false, _ -> Trial.ofError (path, "Foutieve formattering")

        let validateMandatory (path: string) (opt: 'a option) =
            match opt with
            | Some opt -> Trial.Pass opt
            | None -> Trial.ofError (path, "Verplicht veld")

        trial {
            from amount in validateDecimal (nameof me.Amount) me.Amount
            also bankAccount in validateMandatory (nameof me.FromBankAccount) me.FromBankAccount
            also financialCategoryId in validateMandatory (nameof me.FinancialCategoryId) me.FinancialCategoryId
            yield {
                InvoicePayment.InvoiceId = me.InvoiceId
                BuildingId = me.BuildingId
                InvoicePaymentId = me.InvoicePaymentId
                Amount = amount
                Date = me.Date
                FromBankAccount = bankAccount
                FinancialCategoryId = financialCategoryId
                MediaFiles = me.MediaFiles
            }
        }