module Server.Organizations.Query

open System
open Npgsql.FSharp
open Server.PostgreSQL
open Server.PostgreSQL.Sql
open Server.Library
open Shared.Read
open Server.Addresses.Library
open Server.ContactMethods.Library
open FSharp.Data.Runtime

[<AutoOpen>]
module private Readers =
    type OrganizationDbModel = {
        OrganizationId: Guid
        BuildingId: Guid option
        OrganizationNumber: string option
        VatNumber: string option
        VatNumberVerifiedOn: DateTime option
        Name: string
        Address: string
        MainEmailAddress: string option
        MainEmailAddressComment: string option
        MainTelephoneNumber: string option
        MainTelephoneNumberComment: string option
        OtherContactMethods: string option
        BankAccounts: string option
    }

    let readOrganization (reader: CaseInsensitiveRowReader): OrganizationDbModel = {
        OrganizationId = reader.uuid "OrganizationId"
        BuildingId = reader.uuidOrNone "BuildingId"
        OrganizationNumber = reader.stringOrNone "OrganizationNumber"
        VatNumber = reader.stringOrNone "VatNumber"
        VatNumberVerifiedOn = reader.dateTimeOrNone "VatNumberVerifiedOn"
        Name = reader.string "Name"
        Address = reader.string "Address"
        MainEmailAddress = reader.stringOrNone "MainEmailAddress"
        MainEmailAddressComment = reader.stringOrNone "MainEmailAddressComment"
        MainTelephoneNumber = reader.stringOrNone "MainTelephoneNumber"
        MainTelephoneNumberComment = reader.stringOrNone "MainTelephoneNumberComment"
        OtherContactMethods = reader.stringOrNone "OtherContactMethods"
        BankAccounts = reader.stringOrNone "BankAccounts"
    }

    type OrganizationListItemDbModel = {
        OrganizationId: Guid
        BuildingId: Guid option
        VatNumber: string option
        OrganizationNumber: string option
        Name: string
        Address: string
        MainEmailAddress: string option
        MainTelephoneNumber: string option
        BankAccounts: string option
    }

    let readOrganizations (reader: CaseInsensitiveRowReader): OrganizationListItemDbModel = {
        OrganizationId = reader.uuid "OrganizationId"
        BuildingId = reader.uuidOrNone "BuildingId"
        VatNumber = reader.stringOrNone "VatNumber"
        OrganizationNumber = reader.stringOrNone "OrganizationNumber"
        Name = reader.string "Name"
        Address = reader.string "Address"
        MainEmailAddress = reader.stringOrNone "MainEmailAddress"
        MainTelephoneNumber = reader.stringOrNone "MainTelephoneNumber"
        BankAccounts = reader.stringOrNone "BankAccounts"
    }

    type ContactPersonDbModel = {
        OrganizationId: Guid
        BuildingId: Guid option
        PersonId: Guid
        RoleWithinOrganization: string
    }

    let readContactPerson (reader: CaseInsensitiveRowReader): ContactPersonDbModel = {
        OrganizationId = reader.uuid "OrganizationId"
        BuildingId = reader.uuidOrNone "BuildingId"
        PersonId = reader.uuid "PersonId"
        RoleWithinOrganization = reader.string "RoleWithinOrganization"
    }
        

let getContactPersonsForOrganization (connectionString: string) (organizationId: Guid) = async {
    let! contactPersons =
        Sql.connect connectionString
        |> Sql.query
            """
                SELECT 
                    cp.OrganizationId,
                    o.BuildingId,
                    cp.PersonId,
                    cp.RoleWithinOrganization
                FROM ContactPersons cp
                LEFT JOIN Organizations o on o.OrganizationId = cp.OrganizationId
                WHERE cp.OrganizationId = @OrganizationId
            """
        |> Sql.parameters [ "@OrganizationId", Sql.uuid organizationId ]
        |> Sql.read readContactPerson

    let personIds = contactPersons |> List.map (fun r -> r.PersonId)
    let! persons = Server.Persons.Query.getPersonsByIds connectionString personIds

    return
        contactPersons |> List.map (fun cp -> {
            //Referential constraint makes it so there will ALWAYS be a person for every contact person.
            Person = persons |> List.find (fun p -> cp.PersonId = p.PersonId)
            OrganizationId = cp.OrganizationId
            BuildingId = cp.BuildingId
            RoleWithinOrganization = cp.RoleWithinOrganization 
        })
}

let private inMemoryCache = Caching.createInMemoryCache (TimeSpan.FromMinutes 15.0) 

let clearCache () =
    inMemoryCache.Remove ("OrganizationTypes")

let getOrganizationTypes (connectionString: string) () =
    match inMemoryCache.TryRetrieve ("OrganizationTypes") with
    | Some organizationTypes -> Async.lift organizationTypes
    | None -> 
        async {
            let! organizationTypes =
                Sql.connect connectionString
                |> Sql.query "SELECT OrganizationTypeId, Name FROM OrganizationTypes"
                |> Sql.parameters []
                |> Sql.read (fun reader -> { OrganizationTypeId = reader.uuid "OrganizationTypeId"; Name = reader.string "Name" })
                |> Async.map (List.map (fun organizationType -> organizationType.OrganizationTypeId, organizationType) >> dict)
            inMemoryCache.Set ("OrganizationTypes", organizationTypes)
            return organizationTypes
        }

let getOrganizationTypesForOrganizations (connectionString: string) (organizationIds: Guid list) = async {
    let! allOrganizationTypes = getOrganizationTypes connectionString ()

    let! links =
        Sql.connect connectionString
        |> Sql.query "SELECT OrganizationId, OrganizationTypeId FROM OrganizationOrganizationTypeLinks WHERE OrganizationId = ANY (@OrganizationIds)"
        |> Sql.parameters [ "@OrganizationIds", Sql.uuidArray (organizationIds |> List.toArray) ]
        |> Sql.read (fun reader ->
            let organizationId = reader.uuid "OrganizationId"
            let organizationTypeId = reader.uuid "OrganizationTypeId"
            let organizationType = 
                match allOrganizationTypes.TryGetValue(organizationTypeId) with
                | true, t -> Some t
                | false, _ -> None
            {| OrganizationId = organizationId; OrganizationType = organizationType |})

    return
        links
        |> List.groupBy (fun l -> l.OrganizationId)
        |> List.map (fun (organizationId, x) -> organizationId, x |> List.choose (fun y -> y.OrganizationType))
}

let getOrganizationTypesForOrganization (connectionString: string) (organizationId: Guid) =
    getOrganizationTypesForOrganizations connectionString [ organizationId ]
    |> Async.map (function | [ x ] -> snd x | _ -> [])

//Can probably be simplified...
let getOrganization (connectionString: string) (organizationId: Guid) = async {
    let! result =
        Sql.connect connectionString
        |> Sql.query
            """
                SELECT
                    OrganizationId,
                    BuildingId,
                    OrganizationNumber,
                    VatNumber,
                    VatNumberVerifiedOn,
                    Name,
                    Address,
                    MainEmailAddress,
                    MainEmailAddressComment,
                    MainTelephoneNumber,
                    MainTelephoneNumberComment,
                    OtherContactMethods,
                    BankAccounts
                FROM Organizations
                WHERE OrganizationId = @OrganizationId
            """
        |> Sql.parameters [ "@OrganizationId", Sql.uuid organizationId ]
        |> Sql.readSingle readOrganization

    match result with
    | Some dbModel ->
        let forceAddress =
            Address.fromJson
            >> Option.fromResult
            >> Option.defaultValue (Address.Init ())

        let otherContactMethods =
            match dbModel.OtherContactMethods with
            | Some str -> ContactMethod.listFromJson str |> Option.fromResult |> Option.defaultValue []
            | None     -> []

        let! contactPersons = getContactPersonsForOrganization connectionString organizationId
        let! organizationTypes = getOrganizationTypesForOrganization connectionString organizationId

        let organization: Organization = {
            OrganizationId = dbModel.OrganizationId
            BuildingId = dbModel.BuildingId
            OrganizationTypes = organizationTypes
            OrganizationNumber = dbModel.OrganizationNumber
            VatNumber = dbModel.VatNumber
            VatNumberVerifiedOn = dbModel.VatNumberVerifiedOn
            Name = dbModel.Name
            Address = dbModel.Address |> forceAddress
            ContactPersons = contactPersons
            MainEmailAddress = dbModel.MainEmailAddress
            MainEmailAddressComment = dbModel.MainEmailAddressComment
            MainTelephoneNumber = dbModel.MainTelephoneNumber
            MainTelephoneNumberComment = dbModel.MainTelephoneNumberComment
            OtherContactMethods = otherContactMethods
            BankAccounts = dbModel.BankAccounts |> Option.either BankAccount.listFromJson []
        }
        return Some organization
    | None ->
        return None
}

let getOrganizations connectionString (buildingId: Guid) = async {
    let! organizations =
        Sql.connect connectionString
        |> Sql.query
            """
                SELECT
                    OrganizationId,
                    BuildingId,
                    VatNumber,
                    OrganizationNumber,
                    Name,
                    Address,
                    MainEmailAddress,
                    MainTelephoneNumber,
                    BankAccounts
                FROM Organizations
                WHERE BuildingId = @BuildingId
            """
        |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
        |> Sql.read readOrganizations

    let! organizationTypes = 
        getOrganizationTypesForOrganizations connectionString (organizations |> List.map (fun o -> o.OrganizationId))
        |> Async.map dict
    return
        organizations |> List.map (fun o -> {
            OrganizationId = o.OrganizationId
            BuildingId = o.BuildingId
            VatNumber = o.VatNumber
            OrganizationNumber = o.OrganizationNumber
            OrganizationTypeNames = 
                match organizationTypes.TryGetValue(o.OrganizationId) with
                | true, types -> types |> List.map (fun t -> t.Name)
                | false, _ -> []
            Name = o.Name
            Address = Address.fromJson o.Address |> Option.fromResult |> Option.defaultValue (Address.Init ())
            BankAccounts = o.BankAccounts |> Option.either BankAccount.listFromJson []
        })
}

let getOrganizationsByIds connectionString (orgIds: Guid list) = async {
    match orgIds with
    | [] -> return []
    | orgIds ->
        let! organizations =
            Sql.connect connectionString
            |> Sql.query
                """
                    SELECT
                        OrganizationId,
                        BuildingId,
                        OrganizationNumber,
                        VatNumber,
                        Name,
                        Address,
                        MainEmailAddress,
                        MainTelephoneNumber,
                        BankAccounts
                    FROM Organizations
                    WHERE OrganizationId = ANY (@OrganizationIds)
                """
            |> Sql.parameters [ "@OrganizationIds", Sql.uuidArray (orgIds |> List.toArray) ]
            |> Sql.read readOrganizations

        let! organizationTypes = 
            getOrganizationTypesForOrganizations connectionString (organizations |> List.map (fun o -> o.OrganizationId))
            |> Async.map dict
        return
            organizations |> List.map (fun o -> {
                OrganizationId = o.OrganizationId
                BuildingId = o.BuildingId
                VatNumber = o.VatNumber
                OrganizationNumber = o.OrganizationNumber
                OrganizationTypeNames = 
                    match organizationTypes.TryGetValue(o.OrganizationId) with
                    | true, types -> types |> List.map (fun t -> t.Name)
                    | false, _ -> []
                Name = o.Name
                Address = Address.fromJson o.Address |> Option.fromResult |> Option.defaultValue (Address.Init ())
                BankAccounts = o.BankAccounts |> Option.either BankAccount.listFromJson []
            })
}
    