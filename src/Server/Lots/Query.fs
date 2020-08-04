module Server.Lots.Query

open System
open Shared.Read
open Shared.Library
open Server.Library
open Npgsql.FSharp
open Server.PostgreSQL
open Server.PostgreSQL.Sql

[<AutoOpen>]
module private Readers =
    type LotDbModel = {
        LotId: Guid
        BuildingId: Guid
        Code: string
        LotType: string
        Description: string option
        Floor: int option
        Surface: int option
    }

    let readLot (reader: CaseInsensitiveRowReader): LotDbModel = {
        LotId = reader.uuid "LotId"
        BuildingId = reader.uuid "BuildingId"
        Code = reader.string "Code"
        LotType = reader.string "LotType"
        Description = reader.stringOrNone "Description"
        Floor = reader.intOrNone "Floor"
        Surface = reader.intOrNone "Surface"
    }

    type LotOwnerDbModel = {
        LotId: Guid
        PersonId: Guid option
        OrganizationId: Guid option
        Role: LotOwnerRole
    }

    let readLotOwner (reader: CaseInsensitiveRowReader): LotOwnerDbModel = {
        LotId = reader.uuid "LotId"
        OrganizationId = reader.uuidOrNone "OrganizationId"
        PersonId = reader.uuidOrNone "PersonId"
        Role =
            match reader.string "Role" with
            | x when x = string LotOwnerRole.LegalRepresentative -> LotOwnerRole.LegalRepresentative
            | _                                                  -> LotOwnerRole.Other
    }

    let readLegalRepresentative (reader: CaseInsensitiveRowReader): LotOwnerListItem option =
        let personId = reader.uuidOrNone "LegalRepresentativePersonId"
        let firstName = reader.stringOrNone "LegalRepresentativePersonFirstName"
        let lastName = reader.stringOrNone "LegalRepresentativePersonLastName"
        match personId, firstName, lastName with
        | Some pId, Some fName, Some lName -> Some (LotOwnerListItem.Owner {| PersonId = pId; Name = sprintf "%s %s" fName lName |})
        | _ ->
            let orgId = reader.uuidOrNone "LegalRepresentativeOrgId"
            let orgName = reader.stringOrNone "LegalRepresentativeOrgName"
            match orgId, orgName with
            | Some orgId, Some orgName -> Some (LotOwnerListItem.Organization {| OrganizationId = orgId; Name = orgName |})
            | _ -> None

    let readLotListItem (reader: CaseInsensitiveRowReader): LotListItem = {
        LotId = reader.uuid "LotId"
        BuildingId = reader.uuid "BuildingId"
        Code = reader.string "Code"
        LotType = LotType.OfString (reader.string "LotType")
        Description = reader.stringOrNone "Description"
        Floor = reader.intOrNone "Floor"
        LegalRepresentative = readLegalRepresentative reader
    }

let getLot (connectionString: string) (lotId: Guid): Async<Lot option> = async {
    let! lotDbModel =
        Sql.connect connectionString
        |> Sql.query
            """
                SELECT
                    LotId,
                    BuildingId,
                    Code,
                    LotType,
                    Description,
                    Floor,
                    Surface
                FROM Lots
                WHERE LotId = @LotId
            """
        |> Sql.parameters [ "@LotId", Sql.uuid lotId ]
        |> Sql.readSingle readLot

    match lotDbModel with
    | Some dbModel ->
        let! ownerRoles =
            Sql.connect connectionString
            |> Sql.query "SELECT lotOwner.LotId, lotOwner.OrganizationId, lotOwner.PersonId, lotOwner.Role FROM LotOwners lotOwner WHERE lotId = @LotId"
            |> Sql.parameters [ "@LotId", Sql.uuid lotId ]
            |> Sql.read readLotOwner

        let personOwnerRoles, orgOwnerRoles = 
            ownerRoles
            |> List.partition (fun lotOwnerId -> lotOwnerId.PersonId.IsSome)
            |> (fun (personOwnerRoles, orgOwnerRoles) -> 
                personOwnerRoles |> List.map (fun ownerAndRole -> (ownerAndRole.PersonId.Value, ownerAndRole.Role)) |> dict,
                orgOwnerRoles    |> List.map (fun ownerAndRole -> (ownerAndRole.OrganizationId.Value, ownerAndRole.Role)) |> dict
            )


        let getRoleForLotOwnerPerson (lotOwner: OwnerListItem) =
            personOwnerRoles.TryFind lotOwner.PersonId
            |> Option.defaultValue LotOwnerRole.Other

        let getRoleForLotOwnerOrganization (lotOwner: OrganizationListItem) =
            orgOwnerRoles.TryFind lotOwner.OrganizationId
            |> Option.defaultValue LotOwnerRole.Other

        let! ownerPersons =
            personOwnerRoles.Keys
            |> List.ofSeq
            |> Server.Owners.Query.getOwnersByIds connectionString
            |> Async.map (fun list -> list |> List.map (fun li -> LotOwner.Owner li, getRoleForLotOwnerPerson li))

        let! ownerOrgs =
            orgOwnerRoles.Keys
            |> List.ofSeq
            |> Server.Organizations.Query.getOrganizationsByIds connectionString
            |> Async.map (fun list -> list |> List.map (fun li -> LotOwner.Organization li, getRoleForLotOwnerOrganization li))

        return Some {
            LotId = dbModel.LotId            
            BuildingId = dbModel.BuildingId
            Code = dbModel.Code
            LotType = LotType.OfString dbModel.LotType
            Description = dbModel.Description
            Floor = dbModel.Floor
            Surface = dbModel.Surface
            Owners = ownerOrgs @ ownerPersons
        }
    | None ->
        return None
}



let getLots (connectionString: string) (filter: {| BuildingId: Guid |}): Async<LotListItem list> =
    Sql.connect connectionString
    |> Sql.query
        (sprintf 
            """
                SELECT
                    lot.LotId,
                    lot.BuildingId,
                    person.PersonId as LegalRepresentativePersonId,
                    person.FirstName as LegalRepresentativePersonFirstName,
                    person.LastName as LegalRepresentativePersonLastName,
                    org.OrganizationId as LegalRepresentativeOrgId,
                    org.Name as LegalRepresentativeOrgName,
                    lot.Code,
                    lot.LotType,
                    lot.Description,
                    lot.Floor
                FROM Lots lot
                LEFT JOIN LotOwners lotOwner on lot.LotId = lotOwner.LotId
                LEFT JOIN Persons person on lotOwner.PersonId = person.PersonId
                LEFT JOIN Organizations org on lotOwner.OrganizationId = org.OrganizationId
                WHERE lot.BuildingId = @BuildingId AND lot.IsActive = TRUE AND lotOwner.Role = '%s'
            """ 
            (string LotOwnerRole.LegalRepresentative))
    |> Sql.parameters [ "@BuildingId", Sql.uuid filter.BuildingId ]
    |> Sql.read readLotListItem