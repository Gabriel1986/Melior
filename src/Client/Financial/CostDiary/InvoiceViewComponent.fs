module Client.Financial.CostDiary.InvoiceViewComponent

open System
open Fable.React
open Shared.Read
open Client.ClientStyle.Helpers
open Client.Upload
open Shared.MediaLibrary

type Props = {|
    Invoice: Invoice
|}

let view (props: Props) =
    let detail = props.Invoice
    div [] [
        fieldset [] [
            legend [] [ h2 [] [ str "Algemeen" ] ]
            readonlyFormElement "BoekingsNummer" detail.LocalInvoiceNumber
            readonlyFormElement "Boekingsdatum" (detail.BookingDate.ToString("dd/MM/yyyy"))
            readonlyFormElement "Rubriek" (sprintf "%s - %s" detail.CategoryCode detail.CategoryDescription)
            readonlyFormElement "Omschrijving" (detail.Description |> Option.defaultValue "")
        ]
        fieldset [] [            
            legend [] [ h2 [] [ str "Leverancier" ] ]
            readonlyFormElement "Naam" detail.OrganizationName
            match detail.OrganizationNumber, detail.OrganizationVatNumber with
            | Some orgNr, _ ->
                readonlyFormElement "Ondernemingsnr" orgNr
            | _, Some vatNr ->
                readonlyFormElement "BTW nr." vatNr
            | _ ->
                null
        ]
        fieldset [] [
            legend [] [ h2 [] [ str "Factuur" ] ]
            readonlyFormElement "Nr." (detail.ExternalInvoiceNumber |> Option.defaultValue "")
            readonlyFormElement "OpmaakDatum" (detail.InvoiceDate.ToString("dd/MM/yyyy"))
            readonlyFormElement "Einddatum betaling" (detail.DueDate.ToString("dd/MM/yyyy"))
            readonlyFormElement "Bedrag" (String.Format("{0:0.00}", detail.Cost).Replace('.', ','))
            readonlyFormElement "Naar rekening" (string detail.ToBankAccount)
        ]
        fieldset [] [
            legend [] [ h2 [] [ str "Betaling" ] ]
            readonlyFormElement "Van rekening" (string detail.FromBankAccount)
            readonlyFormElement "Met verdeelsleutel" detail.DistributionKey.Name
            readonlyFormElement "Reeds betaald?" (if detail.PaymentIds.IsEmpty then "Nee" else "Ja")
        ]
        fieldset [] [
            legend [] [ h2 [] [ str "Gekoppelde documenten" ] ]
            div [] [ str "TODO." ]
        ]
    ]

let render =
    FunctionComponent.Of ((fun (props: Props) -> view props), memoizeWith = memoEqualsButFunctions)