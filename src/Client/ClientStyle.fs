module Client.ClientStyle

open Zanaptak.TypedCssClasses

type Bootstrap = CssClasses<"public/styles/bootstrap.min.css", Naming.CamelCase, resolutionFolder=__SOURCE_DIRECTORY__>
type FontAwesome = CssClasses<"public/styles/fontawesome.all.min.css", Naming.CamelCase, resolutionFolder=__SOURCE_DIRECTORY__>

module Flatpickr =
    open System

    /// Sets the initial value for the Flatpickr component
    let Values (dates: DateTime list) = 
        {| Value = dates; IsConfig = false; Key = "values" |}
        |> unbox<Flatpickr.IFlatpickrOption>

module Helpers =
    open System
    open Fable.React
    open Fable.React.Props

    let classes (classList: string list) = Class (String.Join(" ", classList))
    
    type FormGroupProp =
        | Name of string
        | Label of string
        | Input of IHTMLProp seq
        | TextArea of IHTMLProp seq
        | Select of FormSelect
        | Radio of FormRadio
        | Date of Flatpickr.IFlatpickrOption list
        | FormError of string option
    and FormSelect = {
        Identifier: string
        OnChanged: string -> unit
        Options: FormSelectOption list
    }
    and FormSelectOption = {
        Key: string
        Label: string
        IsSelected: bool
    }
    and FormRadio = {
        Inline: bool
        RadioButtons: FormRadioButton list
    }
    and FormRadioButton = {
        Id: string
        Key: string
        Label: string
        IsSelected: bool
        OnClick: string -> unit
    }

    let private formRadio (props: FormRadio) =
        let toRadio (radio: FormRadioButton) =
            div [ 
                    classes [ yield Bootstrap.formCheck; if props.Inline then yield Bootstrap.formCheckInline ]
                    OnClick (fun _ -> radio.OnClick radio.Key)
                ] [
                    yield
                        input [ 
                            Class Bootstrap.formCheckInput
                            Type "radio"
                            Id radio.Id
                            Value radio.Key
                            Checked radio.IsSelected 
                        ]
                    if not (String.IsNullOrWhiteSpace(radio.Label)) then
                        yield
                            label [ 
                                Class Bootstrap.formCheckLabel
                                HtmlFor radio.Id 
                            ] [ str radio.Label ]
                ]

        div [] (props.RadioButtons |> List.map toRadio)

    let private formSelect (props: FormSelect) =
        let toOption (opt: FormSelectOption) =
            option [ Value opt.Key; Selected opt.IsSelected ] [ str opt.Label ]

        select [ Class Bootstrap.formControl; Id props.Identifier; OnChange (fun e -> props.OnChanged e.Value) ] [
            yield! props.Options |> List.map toOption
        ]

    let formGroup (props: FormGroupProp list) =
        let name = props |> List.tryPick (function | Name x -> Some x | _ -> None)
        let lbl = props |> List.tryPick (function | Label x -> Some x | _ -> None)
        let inputAttributes = props |> List.tryPick (function | Input x -> Some x | _ -> None)
        let textAreaAttributes = props |> List.tryPick (function | TextArea x -> Some x | _ -> None)
        let selectProps = props |> List.tryPick (function | Select x -> Some x | _ -> None)
        let radioProps = props |> List.tryPick (function | Radio x -> Some x | _ -> None)
        let dateProps = props |> List.tryPick (function | Date x -> Some x | _ -> None)
        let error = props |> List.tryPick (function | FormError e -> e | _ -> None)

        let theName = name |> Option.orElse lbl |> Option.defaultValue ""

        div [ Class Bootstrap.formGroup ] [
            if lbl.IsSome then
                yield label [ HtmlFor theName ] [ str lbl.Value ]
            if inputAttributes.IsSome then
                yield input (Seq.append inputAttributes.Value [ classes [ yield Bootstrap.formControl; if error.IsSome then yield Bootstrap.isInvalid ] ])
            if textAreaAttributes.IsSome then
                yield textarea (Seq.append textAreaAttributes.Value [ classes [ yield Bootstrap.formControl; if error.IsSome then yield Bootstrap.isInvalid ] ]) []
            if selectProps.IsSome then
                yield formSelect selectProps.Value
            if radioProps.IsSome then
                yield formRadio radioProps.Value
            if dateProps.IsSome then
                yield
                    Flatpickr.flatpickr (dateProps.Value @ [ 
                        Flatpickr.ClassName Bootstrap.formControl
                        Flatpickr.Locale Flatpickr.Locales.dutch 
                    ])
            if error.IsSome then
                yield
                    div [ Class Bootstrap.invalidFeedback ] [ str error.Value ]
        ]

    let readonlyFormElement' (lbl: string) (value: string) (description: string) =
        if (String.IsNullOrWhiteSpace(value)) then
            str ""
        else
            div [ Class Bootstrap.row ] [
                yield label [ classes [ Bootstrap.colMd4; Bootstrap.colLg3; Bootstrap.fontWeightBold ] ] [ str lbl ]
                yield p [ Class Bootstrap.col ] [ str value ]
                yield p [ Class Bootstrap.col ] [ str description ]            
            ]

    let readonlyFormElement (lbl: string) (value: string) =
        readonlyFormElement' lbl value ""