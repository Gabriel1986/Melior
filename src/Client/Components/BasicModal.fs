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
        | ModalSize of ModalSize
    and HeaderProp =
        | Title of string
        | HasDismissButton of bool
    and FooterProp =
        | Buttons of ReactElement list
        | ShowDismissButton of string option
    and ModalSize =
        | SmallSize
        | MediumSize
        | LargeSize

    [<AutoOpen>]
    module private Internals =
        let headerClose onClose =
            button [ Type "button"; classes [ Bootstrap.floatRight; Bootstrap.close ]; OnClick onClose ] [
                span [] [ str "×" ]
            ]

        let headerTitle title (onDismiss: (unit -> unit) option) =
            h5 [ classes [ Bootstrap.cardTitle; Bootstrap.dInline ] ] [ 
                yield str title 
                if onDismiss.IsSome then yield headerClose (fun _ -> onDismiss.Value())
            ]

        let footerClose onClose label =
            button 
                [ Type "button"; classes [ Bootstrap.btn; Bootstrap.btnOutlineDanger ]; OnClick onClose ] 
                [ str label ]

        let header (onDismiss: unit -> unit) (headerProps: HeaderProp list) =
            let title = headerProps |> List.tryPick (function | Title x -> Some x | _ -> None)
            let showDismissButton = 
                headerProps 
                |> List.tryPick (function | HeaderProp.HasDismissButton x -> Some x | _ -> None)
                |> Option.defaultValue false

            div [ Class Bootstrap.cardHeader ] [                
                if title.IsSome || showDismissButton then 
                    yield headerTitle (title |> Option.defaultValue "") (if showDismissButton then Some onDismiss else None)
            ]

        let body children =
            div [ Class Bootstrap.cardBody ] [ yield! children ]

        let footer (onDismiss: unit -> unit) (footerProps: FooterProp list) =
            let buttons = footerProps |> List.tryPick (function | Buttons x -> Some x | _ -> None)
            let dismissButton = footerProps |> List.tryPick (function | FooterProp.ShowDismissButton x -> x | _ -> None)

            div [ classes [ Bootstrap.cardFooter; Bootstrap.textRight ] ] [
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

            let size =
                modalProps
                |> List.tryPick (function | ModalSize x -> Some x | _ -> None)
                |> Option.defaultValue MediumSize

            let onBackgroundClick =
                if disableBackgroundClick then
                    OnClick (fun _ -> ())
                else
                    OnClick (fun e -> if e.target = e.currentTarget then onDismiss())

            if isShowing then
                div [ classes [ "melior-modal-grid"; match size with | LargeSize -> "melior-modal-grid-lg" | SmallSize -> "melior-modal-grid-sm" | MediumSize -> null ] ] [
                    div [ Class "melior-modal-background-up"; onBackgroundClick ] []
                    div [ Class "melior-modal-background-left"; onBackgroundClick ] []
                    div [ Class "melior-modal" ] [
                        div [
                            Class Bootstrap.card
                        ] [
                            header onDismiss headerProps
                            body content
                            footer onDismiss footerProps
                        ]
                    ]
                    div [ Class "melior-modal-background-right"; onBackgroundClick ] []
                    div [ Class "melior-modal-background-down"; onBackgroundClick ] []
                ]
            else
                null

    let render (props: {| ModalProps: ModalProp list |}) =
       modal props