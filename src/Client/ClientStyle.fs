module Client.ClientStyle

open Zanaptak.TypedCssClasses
open Shared.Read

type Bootstrap = CssClasses<"public/styles/bootstrap.min.css", Naming.CamelCase, resolutionFolder=__SOURCE_DIRECTORY__>
type FontAwesome = CssClasses<"public/styles/fontawesome.all.min.css", Naming.CamelCase, resolutionFolder=__SOURCE_DIRECTORY__>

module DateTime =
    open Fable.Core
    open Fable.DateFunctions

    /// Hack
    module Locales =
        [<Import("nl", "date-fns/locale")>]
        let Dutch: ILocale = jsNative

module Flatpickr =
    open System
    open Fable.Core

    /// Sets the initial value for the Flatpickr component
    let Values (dates: DateTime array) = 
        {| Value = dates; IsConfig = false; Key = "value" |}
        |> unbox<Flatpickr.IFlatpickrOption>

    /// Hack
    module Locales =
        [<Import("Dutch", "flatpickr/dist/l10n/nl.js")>]
        let dutch: Flatpickr.IFlatpickrLocale = jsNative

module Helpers =
    open System
    open Fable.React
    open Fable.React.Props

    let classes (classList: string list) = Class (String.Join(" ", classList))
    
    type FormGroupProp =
        | Name of string
        | Label of string
        | InputPrepend of ReactElement seq
        | Input of IHTMLProp seq
        | InputAppend of ReactElement seq
        | TextArea of IHTMLProp seq
        | Select of FormSelect
        | Radio of FormRadio
        | Date of Flatpickr.IFlatpickrOption list
        | OtherChildren of ReactElement seq
        | FieldError of string option
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
    and FormRadioButton = 
        {
            Id: string
            Key: string
            Label: string
            IsSelected: bool
            OnClick: string -> unit
        }
        static member YesNo (yes: bool) (onChange: bool -> unit) = [
            {
                Id = "yes"
                Key = "Yes"
                Label = "Ja"
                IsSelected = yes
                OnClick = (fun _ -> onChange true)
            }
            {
                Id = "no"
                Key = "No"
                Label = "Nee"
                IsSelected = not yes
                OnClick = (fun _ -> onChange false)
            }
        ]

    let private formRadio (props: FormRadio) (hasError: bool) =
        let toRadio (radio: FormRadioButton) =
            div [ 
                    classes [ yield Bootstrap.formCheck; if props.Inline then yield Bootstrap.formCheckInline ]
                    OnClick (fun _ -> if radio.IsSelected then () else radio.OnClick radio.Key)
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

        div [ if hasError then yield Class Bootstrap.isInvalid ] (props.RadioButtons |> List.map toRadio)

    let private formSelect (props: FormSelect) (hasError: bool) =
        let selected = props.Options |> List.tryFind (fun opt -> opt.IsSelected)
        let toOption (opt: FormSelectOption) =
            option [ Value opt.Key ] [ str opt.Label ]

        select [ classes [ yield Bootstrap.formControl; if hasError then yield Bootstrap.isInvalid ]; Id props.Identifier; Value (selected |> Option.map (fun s -> s.Key) |> Option.defaultValue "") ; OnChange (fun e -> props.OnChanged e.Value) ] [
            yield! props.Options |> List.map toOption
        ]

    let formGroup (props: FormGroupProp list) =
        let name = props |> List.tryPick (function | Name x -> Some x | _ -> None)
        let lbl = props |> List.tryPick (function | Label x -> Some x | _ -> None)
        let inputPrepend = props |> List.tryPick (function | InputPrepend x -> Some x | _ -> None)
        let inputAttributes = props |> List.tryPick (function | Input x -> Some x | _ -> None)
        let inputAppend = props |> List.tryPick (function | InputAppend x -> Some x | _ -> None)
        let textAreaAttributes = props |> List.tryPick (function | TextArea x -> Some x | _ -> None)
        let selectProps = props |> List.tryPick (function | Select x -> Some x | _ -> None)
        let radioProps = props |> List.tryPick (function | Radio x -> Some x | _ -> None)
        let dateProps = props |> List.tryPick (function | Date x -> Some x | _ -> None)
        let otherChildren = props |> List.tryPick (function | OtherChildren x -> Some x | _ -> None)
        let error = props |> List.tryPick (function | FieldError e -> e | _ -> None)

        let theName = name |> Option.orElse lbl |> Option.defaultValue ""

        div [ Class Bootstrap.formGroup ] [
            if lbl.IsSome then
                yield label [ HtmlFor theName ] [ str lbl.Value ]
            if inputAttributes.IsSome then
                let inputElement = input (Seq.append inputAttributes.Value [ classes [ yield Bootstrap.formControl; if error.IsSome then yield Bootstrap.isInvalid ] ])
                if inputPrepend.IsSome || inputAppend.IsSome then
                    yield div [ Class Bootstrap.inputGroup ] [
                        if inputPrepend.IsSome then
                            div [ Class Bootstrap.inputGroupPrepend ] inputPrepend.Value
                        inputElement 
                        if inputAppend.IsSome then
                            div [ Class Bootstrap.inputGroupAppend ] inputAppend.Value
                    ]
                else
                    yield inputElement
            if textAreaAttributes.IsSome then
                yield textarea (Seq.append textAreaAttributes.Value [ classes [ yield Bootstrap.formControl; if error.IsSome then yield Bootstrap.isInvalid ] ]) []
            if selectProps.IsSome then
                yield formSelect selectProps.Value (error.IsSome)
            if radioProps.IsSome then
                yield formRadio radioProps.Value (error.IsSome)
            if dateProps.IsSome then
                yield
                    Flatpickr.flatpickr ([
                        Flatpickr.DateFormat "j F, Y"
                        Flatpickr.ClassName "flatpickr-input"
                        Flatpickr.Locale Flatpickr.Locales.dutch
                        Flatpickr.TimeTwentyFour true
                        Flatpickr.EnableTimePicker true
                    ]
                    
                    
                    @ dateProps.Value)
            if otherChildren.IsSome then
                yield! otherChildren.Value
            if error.IsSome then
                yield
                    div [ Class Bootstrap.invalidFeedback ] [ str error.Value ]
        ]

    let readonlyFormElement' (lbl: string) (value: string) (description: string) =
        if (String.IsNullOrWhiteSpace(value)) then
            str ""
        else
            div [ Class Bootstrap.row ] [
                label [ classes [ Bootstrap.colMd4; Bootstrap.fontWeightBold; Bootstrap.pl0 ]; Style [ MaxWidth "300px" ] ] [ str lbl ]
                p [ Class Bootstrap.col ] [ str value ]
                if not (String.IsNullOrWhiteSpace(description)) then
                    p [ Class Bootstrap.col ] [ str description ]
            ]

    let readonlyFormElement (lbl: string) (value: string) =
        readonlyFormElement' lbl value ""

    let withPageHeader (pageHeader: string) (page: ReactElement) =
        div [ Class Bootstrap.card ] [
            div [ Class Bootstrap.cardHeader ] [
                h3 [] [ str pageHeader ]
            ]
            div [ Class Bootstrap.cardBody ] [
                page
            ]
        ]

    let renderWarnings (warnings: Warning list) =
        match warnings with
        | [] -> null
        | warnings ->
            div [ Class Bootstrap.col12 ] [
                for warning in warnings do
                    yield div [ classes [ Bootstrap.alert; Bootstrap.alertWarning ] ] [ 
                        i [ classes [ FontAwesome.fa; FontAwesome.faExclamationTriangle ] ] []
                        str " "
                        str warning.Message 
                    ]
            ]
