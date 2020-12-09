module Server.Contracts.ContractSystem

open Microsoft.Extensions.Configuration
open Server.Blueprint.Behavior.Contracts
open Server.Blueprint.Behavior.Storage
open Server.AppSettings
open Server.Library
open Server.LibraryExtensions

let build (config: IConfiguration) (store: IStorageEngine) =
    let settings = config.Get<AppSettings>()
    let conn = settings.Database.Connection
    {
        new IContractSystem with
            member _.CreateContract msg = Workflow.createContract store msg
            member _.UpdateContract msg = Workflow.updateContract store msg
            member _.DeleteContract msg = Workflow.deleteContract store msg
            member _.SaveContractTypeAnswers msg = Workflow.saveContractTypeAnswer store msg

            member _.GetContracts msg = async {
                if msg.CurrentUser.HasAccessToBuilding msg.Payload
                then return! Query.getContracts conn msg.Payload
                else return []
            }

            member _.GetContractTypeAnswers msg = async {
                if msg.CurrentUser.HasAccessToBuilding msg.Payload
                then return! Query.getContractTypeAnswers conn msg.Payload
                else return []
            }
    }