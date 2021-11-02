module Client.Financial.Invoicing.InvoicePaymentTypes

open System
open Shared.Read
open Shared.Write

type CreateOrUpdate =
    | Create
    | Update of InvoicePayment

type CreatedOrUpdated =
    | Created of InvoicePayment
    | Updated of InvoicePayment