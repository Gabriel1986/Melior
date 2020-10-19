module Client.Financial.CostDiary.InvoiceViewComponent

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
            legend [] [ h2 [] [ str "Algemeen" ] ]
            readonlyFormElement "BoekingsNummer" detail.LocalInvoiceNumber
            readonlyFormElement "Boekingsdatum" (detail.BookingDate.ToString("dd/MM/yyyy"))
            readonlyFormElement "Rubriek" (sprintf "%s - %s" detail.FinancialCategory.Code detail.FinancialCategory.Description)
            readonlyFormElement "Omschrijving" (detail.Description |> Option.defaultValue "")
            readonlyFormElement "Met verdeelsleutel" detail.DistributionKey.Name
        ]
        fieldset [] [            
            legend [] [ h2 [] [ str "Leverancier" ] ]
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
            legend [] [ h2 [] [ str "Factuur" ] ]
            readonlyFormElement "Nr." (detail.OrganizationInvoiceNumber |> Option.defaultValue "")
            readonlyFormElement "OpmaakDatum" (detail.InvoiceDate.ToString("dd/MM/yyyy"))
            readonlyFormElement "Einddatum betaling" (detail.DueDate |> Option.map (fun d -> d.ToString("dd/MM/yyyy")) |> Option.defaultValue "")
            readonlyFormElement "Bedrag" (String.Format("{0:0.00}", detail.Cost).Replace('.', ','))
            readonlyFormElement "Naar rekening" (string detail.OrganizationBankAccount)
        ]
        match detail.MediaFiles with
        | [] ->
            null
        | mediaFiles ->
            fieldset [] [
                yield legend [] [ h2 [] [ str "Gekoppelde documenten" ] ]
                yield!
                    mediaFiles
                    |> List.map (fun mediaFile -> 
                        div [ Class Bootstrap.formGroup ] [
                            label [] [ str "Document" ]
                            div [ Class Bootstrap.inputGroup ] [
                                a [ Href (downloadUri Partitions.Contracts mediaFile.FileId); Target "_blank"; Class Bootstrap.formControl ] [
                                    str (sprintf "%s (%s)" mediaFile.FileName (mediaFile.FileSizeString ()))
                                ]
                            ]
                        ]
                    )
            ]
    ]

let render =
    FunctionComponent.Of ((fun (props: Props) -> view props), memoizeWith = memoEqualsButFunctions)