#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"

#if !FAKE
#r "netstandard"
#r "Facades/netstandard" // https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095
#endif

open System

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.Tools.Git

Target.initEnvironment ()

let serverPath = Path.getFullName "./src/Server"
let clientPath = Path.getFullName "./src/Client"
let clientDeployPath = Path.combine clientPath "deploy"
let deployDir = Path.getFullName "./deploy"

let release = ReleaseNotes.load "RELEASE_NOTES.md"

let aspnetEnv = defaultArg (Environment.environVarOrNone "aspnet-env") "Development"

let platformTool tool winTool =
    let tool = if Environment.isUnix then tool else winTool
    match ProcessUtils.tryFindFileOnPath tool with
    | Some t -> t
    | _ ->
        let errorMsg =
            tool + " was not found in path. " +
            "Please install it and make sure it's available from your path. " +
            "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
        failwith errorMsg

let nodeTool = platformTool "node" "node.exe"
let npmTool = platformTool "npm" "npm.cmd"
let npxTool = platformTool "npx" "npx.cmd"

let runTool cmd args workingDir =
    let arguments = args |> String.split ' ' |> Arguments.OfArgs
    Command.RawCommand (cmd, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let runDotNet cmd workingDir =
    let result =
        DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

Target.create "Clean" (fun _ ->
    [ deployDir
      clientDeployPath ]
    |> Shell.cleanDirs
)

Target.create "InstallClient" (fun _ ->
    printfn "Node version:"
    runTool nodeTool "--version" __SOURCE_DIRECTORY__
    printfn "Npm version:"
    runTool npmTool "--version"  __SOURCE_DIRECTORY__
    runTool npmTool "i" __SOURCE_DIRECTORY__
)

Target.create "Build" (fun _ ->
    runDotNet "build" serverPath
    Shell.regexReplaceInFileWithEncoding
        "let app = \".+\""
       ("let app = \"" + release.NugetVersion + "\"")
        System.Text.Encoding.UTF8
        (Path.combine clientPath "Version.fs")
    runTool npxTool "webpack-cli -p" __SOURCE_DIRECTORY__
)

Target.create "Publish" (fun _ ->
    let serverDir = Path.combine deployDir "Server"
    // TODO Config dir
    //let configDir = Path.combine deployDir "Configuration"
    let clientDir = Path.combine deployDir "Client"
    let publicDir = Path.combine clientDir "public"

    // TODO add -r win10-x64 --self-contained true
    Environment.setEnvironVar "ASPNETCORE_ENVIRONMENT" "Production"
    let publishArgs = sprintf "publish -c Release -o \"%s\"" serverDir
    runDotNet publishArgs serverPath

    Shell.copyDir publicDir (Path.getFullName "./src/Client/deploy") FileFilter.allFiles
    Shell.copyDir publicDir (Path.getFullName "./src/Client/public") FileFilter.allFiles
    //Shell.copyDir configDir "src/Configuration" (fun s -> s = "src/Configuration/settings.default.json")
)

Target.create "Run" (fun _ ->
    let server = async {
        Environment.setEnvironVar "ASPNETCORE_ENVIRONMENT" aspnetEnv
        runDotNet "watch run -c Debug" serverPath
    }
    let client = async {
        runTool npxTool "webpack-dev-server" __SOURCE_DIRECTORY__
    }

    let tasks =
        [ yield server
          yield client ]

    tasks
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
)

Target.create "RunHeadless" (fun _ ->
    let server = async {
        Environment.setEnvironVar "ASPNETCORE_ENVIRONMENT" aspnetEnv
        runDotNet "watch run -c Debug" serverPath
    }

    let tasks =
        [ yield server ]

    tasks
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
)

open Fake.Core.TargetOperators

"Clean"
    ==> "InstallClient"
    ==> "Build"
    ==> "Publish"

"Clean"
    ==> "InstallClient"
    ==> "Run"

"Clean"
    ==> "RunHeadless"

Target.runOrDefaultWithArguments "Build"