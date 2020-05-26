module Server.Organizations.Workflow

open System
open Server.Library

let deleteOrganization (connectionString: string) (msg: Message<Guid>) = async {
    return Ok ()
}