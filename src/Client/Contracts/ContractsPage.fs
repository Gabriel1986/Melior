module Client.Contracts.ContractsPage

open System
open Elmish
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.ElmishComponents
open Shared.Read
open Shared.MediaLibrary
open Client.Upload

type State = {
    todo: string
    uploaded: MediaFile list
}

type Message =
    | ContractFileUploaded of MediaFile

type ContractsPageProps = 
    {|
        CurrentBuildingId: Guid
        CurrentUser: CurrentUser
    |}

let init props =
    { todo = "Balen"; uploaded = [] }, Cmd.none

let update (msg: Message) (state: State): State * Cmd<Message> =
    match msg with
    | ContractFileUploaded mediaFile ->
        { state with uploaded = mediaFile::state.uploaded }, Cmd.none
    | _ ->
        state, Cmd.none

let partition = "demo-bucket"
let view (state: State) (dispatch: Message -> unit) =
    div [] [
        filePond partition (Guid.NewGuid()) [
            AllowMultiple false
            MaxFiles 1
            OnProcessFile (fun error filePondFile ->
                if String.IsNullOrWhiteSpace(error) then
                    filePondFile 
                    |> FilePondFile.toMediaFile partition (Guid.NewGuid())
                    |> ContractFileUploaded
                    |> dispatch)
        ]
        div [] [
            str "links test: "
            ul [] [
                yield! state.uploaded |> List.map (fun mediaFile ->
                    li [] [
                        p [] [ 
                            str mediaFile.FileName
                            str (string mediaFile.FileSize)
                        ]
                        a [ Href (downloadUri partition mediaFile.FileId) ] [
                            str "DOWNLOAD TEST"
                        ]
                    ]
                )
            ]
        ]
    ]

let render (props: ContractsPageProps) =
    React.elmishComponent ("ContractsPage", init props, update, view)