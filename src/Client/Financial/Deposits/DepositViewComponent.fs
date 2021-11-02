module Client.Financial.Deposits.DepositViewComponent

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
        Deposit: Deposit
        OnEdit: Deposit -> unit
        OnRemove: Deposit -> unit
    |}

let view (props: Props) =
    let detail = props.Deposit

    fieldset [] [
        legend [] [
            div [ Class Bootstrap.formInline ] [
                div [ Class Bootstrap.formGroup ] [
                    h4 [ Class Bootstrap.mr2 ] [ str (sprintf "Betaling #%i" (props.Index + 1)) ]
                    button [ 
                        classes [ Bootstrap.btn; Bootstrap.btnOutlinePrimary; Bootstrap.btnSm; Bootstrap.mr2 ]
                        OnClick (fun _ -> props.OnEdit props.Deposit) 
                    ] [ i [ classes [ FontAwesome.fa; FontAwesome.faEdit ] ] [] ]
                    button [
                        classes [ Bootstrap.btn; Bootstrap.btnOutlineDanger; Bootstrap.btnSm ]
                        OnClick (fun _ -> props.OnRemove props.Deposit)
                    ] [ i [ classes [ FontAwesome.fa; FontAwesome.faTrashAlt ] ] [] ]
                ]
            ]
        ]

        readonlyFormElement "Bedrag" (String.Format("€{0:0.00}", detail.Amount).Replace('.', ','))
        readonlyFormElement "Datum" (detail.Date.ToString("dd/MM/yyyy"))
        readonlyFormElement "Betaling door" (detail.LotOwner.LotOwnerType.Name)
        readonlyFormElement "Van rekening" (string detail.FromBankAccount)

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