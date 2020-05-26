namespace Client.Components

open Fable.React
open Fable.React.Props
open Client.ClientStyle
open Client.ClientStyle.Helpers

module BasicModal =
    type ModalProp =
        | IsOpen of bool
        | OnDismiss of (unit -> unit)
        | DisableBackgroundClick of bool
        | Header of HeaderProp list
        | Body of ReactElement list
        | Footer of FooterProp list
    and HeaderProp =
        | Title of string
        | ShowDismissButton of bool
    and FooterProp =
        | Buttons of ReactElement list
        | ShowDismissButton of string option

    [<AutoOpen>]
    module private Internals =
        let headerTitle title =
            h5 [ Class Bootstrap.modalTitle ] [ str title ]

        let headerClose onClose =
            button [ Type "button"; Class Bootstrap.close; OnClick onClose ] [
                span [] [ str "X" ]
            ]

        let footerClose onClose label =
            button 
                [ Type "button"; classes [ Bootstrap.btn; Bootstrap.btnDanger ]; OnClick onClose ] 
                [ str label ]

        let header (onDismiss: unit -> unit) (headerProps: HeaderProp list) =
            let title = headerProps |> List.tryPick (function | Title x -> Some x | _ -> None)
            let showDismissButton = 
                headerProps 
                |> List.tryPick (function | HeaderProp.ShowDismissButton x -> Some x | _ -> None)
                |> Option.defaultValue false

            div [ Class Bootstrap.modalHeader ] [
                    if title.IsSome then yield headerTitle title.Value
                    if showDismissButton then yield headerClose (fun _ -> onDismiss())
                ]

        let body children =
            div [ Class Bootstrap.modalBody ] [ yield! children ]

        let footer (onDismiss: unit -> unit) (footerProps: FooterProp list) =
            let buttons = footerProps |> List.tryPick (function | Buttons x -> Some x | _ -> None)
            let dismissButton = footerProps |> List.tryPick (function | FooterProp.ShowDismissButton x -> x | _ -> None)

            div [ Class Bootstrap.modalFooter ] [
                if dismissButton.IsSome then yield footerClose (fun _ -> onDismiss()) dismissButton.Value
                if buttons.IsSome then yield! buttons.Value
            ]

        let modal (props: {| ModalProps: ModalProp list |}) =
            let modalProps = props.ModalProps

            let isShowing =
                modalProps
                |> List.tryPick (function | IsOpen x -> Some x | _ -> None)
                |> Option.defaultValue false

            let onDismiss = 
                modalProps 
                |> List.tryPick (function | OnDismiss x -> Some x | _ -> None)
                |> Option.defaultValue (fun () -> ())

            let headerProps =
                modalProps
                |> List.tryPick (function | Header props -> Some props | _ -> None)
                |> Option.defaultValue []

            let content =
                modalProps
                |> List.tryPick (function | Body children -> Some children | _ -> None)
                |> Option.defaultValue []

            let footerProps =
                modalProps
                |> List.tryPick (function | Footer props -> Some props | _ -> None)
                |> Option.defaultValue []

            let disableBackgroundClick =
                modalProps
                |> List.tryPick (function | DisableBackgroundClick x -> Some x | _ -> None)
                |> Option.defaultValue false

            div [ 
                classes ([ Bootstrap.modal; Bootstrap.fade; ] @ if isShowing then [ Bootstrap.show; Bootstrap.modalOpen ] else [])
                OnClick (fun _ -> if disableBackgroundClick then () else onDismiss()) 
            ] [ 
                div [ classes [ Bootstrap.modalDialog;  Bootstrap.modalLg ] ] [
                    div [ 
                        Class Bootstrap.modalContent
                        OnClick (fun e -> e.preventDefault(); e.stopPropagation()) 
                    ] [
                        header onDismiss headerProps
                        body content
                        footer onDismiss footerProps
                    ]
                ]
            ]

    let render =
        FunctionComponent.Of (render = (fun p -> modal p), memoizeWith = memoEqualsButFunctions)