module Client.Financial.Invoicing.InvoiceViewComponent

open System
open Fable.React
open Fable.React.Props
open Shared.Read
open Shared.MediaLibrary
open Client.Upload
open Client.ClientStyle
open Client.ClientStyle.Helpers

type Props = {|
    Invoice: Invoice
|}

let view (props: Props) =
    let detail = props.Invoice
    div [] [
        fieldset [] [
            legend [] [ h4 [] [ str "Algemeen" ] ]
            readonlyFormElement "Boekingsnummer" detail.LocalInvoiceNumber
            readonlyFormElement "Boekingsdatum" (detail.BookingDate.ToString("dd/MM/yyyy"))
            readonlyFormElement "Boekhoudkundige rekening" (sprintf "%s - %s" detail.FinancialCategory.Code detail.FinancialCategory.Description)
            readonlyFormElement "Omschrijving" (detail.Description |> Option.defaultValue "")
            readonlyFormElement "Met verdeelsleutel" detail.DistributionKey.Name
        ]
        fieldset [] [            
            legend [] [ h4 [] [ str "Leverancier" ] ]
            readonlyFormElement "Naam" detail.Organization.Name
            match detail.Organization.OrganizationNumber, detail.Organization.VatNumber with
            | Some orgNr, _ ->
                readonlyFormElement "Ondernemingsnr" orgNr
            | _, Some vatNr ->
                readonlyFormElement "BTW nr." vatNr
            | _ ->
                null
        ]
        fieldset [] [
            legend [] [ h4 [] [ str "Factuur" ] ]
            readonlyFormElement "Nr." (detail.OrganizationInvoiceNumber |> Option.defaultValue "")
            readonlyFormElement "Opmaakdatum" (detail.InvoiceDate.ToString("dd/MM/yyyy"))
            readonlyFormElement "Einddatum betaling" (detail.DueDate.ToString("dd/MM/yyyy"))
            readonlyFormElement "Bedrag" (String.Format("€{0:0.00}", detail.Cost).Replace('.', ','))
            readonlyFormElement "Naar rekening" (string detail.OrganizationBankAccount)
        ]
        match detail.MediaFiles with
        | [] ->
            null
        | mediaFiles ->
            fieldset [] [
                yield legend [] [ h4 [] [ str "Gekoppelde documenten" ] ]
                yield!
                    mediaFiles
                    |> List.map (fun mediaFile -> 
                        div [ Class Bootstrap.formGroup ] [
                            label [] [ str "Document" ]
                            div [ Class Bootstrap.inputGroup ] [
                                a [ Href (downloadUri Partitions.Invoices mediaFile.FileId); Target "_blank"; Class Bootstrap.formControl ] [
                                    str (sprintf "%s (%s)" mediaFile.FileName (mediaFile.FileSizeString ()))
                                ]
                            ]
                        ]
                    )
            ]
    ]

let render =
    FunctionComponent.Of ((fun (props: Props) -> view props), memoizeWith = memoEqualsButFunctions)