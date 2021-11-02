module Client.Financial.Deposits.DepositRequestViewComponent

open System
open Fable.React
open Fable.React.Props
open Shared.Read
open Shared.MediaLibrary
open Client.Upload
open Client.ClientStyle
open Client.ClientStyle.Helpers

type Props = {|
    DepositRequest: DepositRequest
|}

let view (props: Props) =
    let detail = props.DepositRequest
    div [] [
        fieldset [] [
            legend [] [ h4 [] [ str "Algemeen" ] ]
            readonlyFormElement "Boekingsnummer" detail.LocalRequestNumber
            readonlyFormElement "Boekingsdatum" (detail.BookingDate.ToString("dd/MM/yyyy"))
            readonlyFormElement "Boekhoudkundige rekening" (sprintf "%s - %s" detail.ToFinancialCategory.Code detail.ToFinancialCategory.Description)
            readonlyFormElement "Omschrijving" (detail.Description |> Option.defaultValue "")
            readonlyFormElement "Verdeelsleutel" detail.DistributionKey.Name
        ]
        fieldset [] [
            legend [] [ h4 [] [ str "Aanvraag details" ] ]
            //readonlyFormElement "Referentie" (match detail.Reference with BelgianOGMReference ref -> ref)
            readonlyFormElement "Aanvraagdatum" (detail.RequestDate.ToString("dd/MM/yyyy"))
            readonlyFormElement "Einddatum betaling" (detail.DueDate.ToString("dd/MM/yyyy"))
            readonlyFormElement "Bedrag" (String.Format("€{0:0.00}", detail.Amount).Replace('.', ','))
            readonlyFormElement "Naar rekening" (string detail.ToBankAccount)
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