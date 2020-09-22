module Client.Files.FilesPage

open System
open Shared.MediaLibrary
open Shared.Read
open Elmish
open Fable.React

type AccessRight = {
    RoleKind: RoleKind
    CanRead: bool
    CanWrite: bool
}

type File = {
    DirectoryId: Guid
    AccessRights: AccessRight list
    MediaFile: MediaFile
}

type Directory = {
    DirectoryId: Guid
    BuildingId: Guid
    ParentDirectory: Guid option
    AccessRights: AccessRight list
    Files: File list
    SubDirectories: Directory list
}

type State = {
    IsLoading: bool
    Directories: Directory list
}

type Msg =
    | LoadDirectories
    | DirectoriesLoaded of Directory list

let init (props: IFilesPageProps) =
    { IsLoading = true; Directories = [] }, Cmd.none

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    state, Cmd.none

let view (currentUser: User) (state: State) (dispatch: unit -> Msg) =
    div [] []

let render (props: ) =
    React.elmishComponent ("PortalPage", init props, update, view)