module Server.Tests.InvoicePaymentWorkflowTests

open Shared.Read
open Server.Financial.Workflow

type InvoicePaymentScenario = {
    Given: InvoicePayment
    When: CUD
    Then: Outcome
}
and CUD =
    | Create of InvoicePayment
and Outcome =
    | Happy
    | Unhappy of CUDError
and CUDError =
    | CUDError