module Client.Financial.Invoicing.InvoicePaymentViewComponent

open System
open Fable.React
open Fable.React.Props
open Shared.Read
open Client.ClientStyle
open Client.Upload
open Client.ClientStyle.Helpers
open Shared.MediaLibrary

type Props = 
    {|
        Index: int
        InvoicePayment: InvoicePayment
        FinancialCategory: FinancialCategory option
        OnEdit: InvoicePayment -> unit
        OnRemove: InvoicePayment -> unit
    |}

let view (props: Props) =
    let detail = props.InvoicePayment
    fieldset [] [
        legend [] [
            div [ Class Bootstrap.formInline ] [
                div [ Class Bootstrap.formGroup ] [
                    h4 [ Class Bootstrap.mr2 ] [ str (sprintf "Betaling #%i" (props.Index + 1)) ]
                    button [ 
                        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary; Bootstrap.btnSm; Bootstrap.mr2 ]
                        OnClick (fun _ -> props.OnEdit props.InvoicePayment) 
                    ] [ i [ classes [ FontAwesome.fa; FontAwesome.faEdit ] ] [] ]
                    button [
                        classes [ Bootstrap.btn; Bootstrap.btnOutlineDanger; Bootstrap.btnSm ]
                        OnClick (fun _ -> props.OnRemove props.InvoicePayment)
                    ] [ i [ classes [ FontAwesome.fa; FontAwesome.faTrashAlt ] ] [] ]
                ]
            ]
        ]

        readonlyFormElement "Bedrag" (String.Format("€{0:0.00}", detail.Amount).Replace('.', ','))
        readonlyFormElement "Datum" (detail.Date.ToString("dd/MM/yyyy"))
        readonlyFormElement "Van rekening" (string detail.FromBankAccount)
        match props.FinancialCategory with
        | None -> null
        | Some financialCategory ->
            readonlyFormElement "Boekhoudkundige rekening" (sprintf "%s - %s" financialCategory.Code financialCategory.Description)

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
                                a [ Href (downloadUri Partitions.InvoicePayments mediaFile.FileId); Target "_blank"; Class Bootstrap.formControl ] [
                                    str (sprintf "%s (%s)" mediaFile.FileName (mediaFile.FileSizeString ()))
                                ]
                            ]
                        ]
                    )
            ]
    ]

let render =
    FunctionComponent.Of ((fun (props: Props) -> view props), memoizeWith = memoEqualsButFunctions)