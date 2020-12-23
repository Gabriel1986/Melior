module Client.Components.FinancialCategorySelectionModal

open System
open Shared.Read
open Client.Components.BasicModal
open Client.Components.SelectionList

type FinancialCategorySelectionModalProps =
    {|
        SelectedCategoryId: Guid option
        FinancialCategories: FinancialCategory list
        Showing: bool
        OnCancelSelection: unit -> unit
        OnSelectionChanged: FinancialCategory option -> unit
        Filter: (FinancialCategory -> bool) option
    |}

let render (props: FinancialCategorySelectionModalProps) =
    BasicModal.render
        {|
            ModalProps = [
                IsOpen props.Showing
                DisableBackgroundClick false
                OnDismiss (fun _ -> props.OnCancelSelection ())
                Header [
                    HeaderProp.Title "Boekhoudkundige rekening selecteren"
                    HeaderProp.HasDismissButton true
                ]
                Body [
                    SelectionList.render (
                        {|
                            SelectionMode = SelectionMode.SingleSelect
                            AllItems =
                                props.FinancialCategories 
                                |> List.filter (props.Filter |> Option.defaultValue (fun _ -> true)) 
                                |> List.sortBy (fun cat -> cat.Code)
                            SelectedItems =
                                match props.SelectedCategoryId with
                                | Some selected -> props.FinancialCategories |> List.filter (fun cat -> cat.FinancialCategoryId = selected)
                                | None -> []
                            OnSelectionChanged = fun selection -> props.OnSelectionChanged (selection |> List.tryHead)
                            ListItemToString = (fun financialCategory -> sprintf "%s - %s" financialCategory.Code financialCategory.Description)
                        |}, "FinancialCategorySelectionList")
                ]
                Footer [
                    //yield FooterProp.Buttons []
                ]
            ]
        |}