module Server.Organizations.Storage

open System
open Npgsql.FSharp
open Server.PostgreSQL
open Shared.Read
open Shared.Write
open Server.Addresses.Workflow
open Server.ContactMethods.Workflow

let private paramsForContactPerson (validated: ValidatedContactPerson) = [
    "@PersonId"              , Sql.uuid   validated.Person.PersonId
    "@OrganizationId"        , Sql.uuid   validated.OrganizationId
    "@RoleWithinOrganization", Sql.string (string validated.RoleWithinOrganization)
]

[<NoComparison; NoEquality>]
type IOrganizationStorage =
    abstract CreateContactPerson: ValidatedContactPerson -> Async<unit>
    abstract UpdateContactPerson: ValidatedContactPerson -> Async<int>
    abstract DeleteContactPerson: BuildingId option * contactPersonId: Guid  -> Async<int>
    abstract CreateOrganization: ValidatedOrganization -> Async<unit>
    abstract UpdateOrganization: ValidatedOrganization -> Async<int>
    abstract DeleteOrganization: BuildingId option * organizationId: Guid  -> Async<int>
    abstract CreateOrganizationType: ValidatedOrganizationType -> Async<unit>
    abstract UpdateOrganizationType: ValidatedOrganizationType -> Async<int>
    abstract DeleteOrganizationType: organizationTypeId: Guid  -> Async<int>

let createContactPerson (connectionString: string) (validated: ValidatedContactPerson) =
    Sql.connect connectionString
    |> Sql.writeBatchAsync [
        Server.Persons.Storage.createQuery, 
            Server.Persons.Storage.paramsFor validated.Person

        """
            INSERT INTO ContactPersons (
                PersonId,
                OrganizationId,
                RoleWithinOrganization
            ) VALUES (
                @PersonId,
                @OrganizationId,
                @RoleWithinOrganization
            )
        """, paramsForContactPerson validated
    ]
    |> Async.Ignore

let updateContactPerson (connectionString: string) (validated: ValidatedContactPerson) =
    Sql.connect connectionString
    |> Sql.writeBatchAsync [
        Server.Persons.Storage.updateQuery,
            Server.Persons.Storage.paramsFor validated.Person

        """
            UPDATE ContactPersons 
            SET RoleWithinOrganization = @RoleWithinOrganization
            WHERE PersonId = @PersonId
        """, paramsForContactPerson validated
    ]
    |> Async.map (List.skip 1 >> List.tryHead >> Option.defaultValue 0)

let deleteContactPerson connectionString (buildingId, personId) =
    Sql.connect connectionString
    |> Sql.query
        """
            UPDATE cp
            SET IsActive = FALSE
            FROM ContactPersons cp
            LEFT JOIN Organizations o on cp.OrganizationId = o.OrganizationId
            WHERE PersonId = @PersonId AND p.BuildingId = @BuildingId
        """
    |> Sql.parameters [ 
        "@PersonId" , Sql.uuid personId
        "@BuildingId", Sql.uuidOrNone buildingId
    ]
    |> Sql.writeAsync

let createQuery =
    """
        INSERT INTO Organizations (
            OrganizationId,
            BuildingId,
            OrganizationNumber,
            VatNumber,
            VatNumberVerifiedOn,
            Name,
            Address,
            MainTelephoneNumber,
            MainTelephoneNumberComment,
            MainEmailAddress,
            MainEmailAddressComment,
            OtherContactMethods
        ) VALUES (
            @OrganizationId,
            @BuildingId,
            @OrganizationNumber,
            @VatNumber,
            @VatNumberVerifiedOn,
            @Name,
            @Address,
            @MainTelephoneNumber,
            @MainTelephoneNumberComment,
            @MainEmailAddress,
            @MainEmailAddressComment,
            @OtherContactMethods
        )
    """

let updateQuery =
    """
        UPDATE Organizations SET
            OrganizationNumber = @OrganizationNumber,
            VatNumber = @VatNumber,
            VatNumberVerifiedOn = @VatNumberVerifiedOn,
            Name = @Name,
            Address = @Address,
            MainTelephoneNumber = @MainTelephoneNumber,
            MainTelephoneNumberComment = @MainTelephoneNumberComment,
            MainEmailAddress = @MainEmailAddress,
            MainEmailAddressComment = @MainEmailAddressComment,
            OtherContactMethods = @OtherContactMethods
        WHERE OrganizationId = @OrganizationId
    """

let paramsFor (validated: ValidatedOrganization) = [
    "@OrganizationId"            , Sql.uuid validated.OrganizationId
    "@BuildingId"                , Sql.uuidOrNone validated.BuildingId
    "@OrganizationNumber"        , Sql.stringOrNone (validated.OrganizationNumber |> Option.map string)
    "@VatNumber"                 , Sql.stringOrNone (validated.VatNumber |> Option.map string)
    "@VatNumberVerifiedOn"       , Sql.timestampOrNone validated.VatNumberVerifiedOn
    "@Name"                      , Sql.string (string validated.Name)
    "@Address"                   , Sql.jsonb (validated.Address |> ValidatedAddress.toJson)
    "@MainTelephoneNumber"       , Sql.stringOrNone (validated.MainTelephoneNumber |> Option.map string) 
    "@MainTelephoneNumberComment", Sql.stringOrNone (validated.MainTelephoneNumberComment |> Option.map string)
    "@MainEmailAddress"          , Sql.stringOrNone (validated.MainEmailAddress |> Option.map string)
    "@MainEmailAddressComment"   , Sql.stringOrNone (validated.MainEmailAddressComment |> Option.map string)
    "@OtherContactMethods"       , Sql.jsonb (validated.OtherContactMethods |> ValidatedContactMethod.listToJson)
]


let createOrganization (connectionString: string) (validated: ValidatedOrganization) =
    Sql.connect connectionString
    |> Sql.query createQuery
    |> Sql.parameters (paramsFor validated)
    |> Sql.writeAsync
    |> Async.Ignore

let updateOrganization (connectionString: string) (validated: ValidatedOrganization) =
    Sql.connect connectionString
    |> Sql.query updateQuery
    |> Sql.parameters (paramsFor validated)
    |> Sql.writeAsync

let deleteOrganization connectionString (buildingId, orgId) =
    Sql.connect connectionString
    |> Sql.query
        """
            UPDATE Organizations 
            SET IsActive = FALSE
            WHERE OrganizationId = @OrganizationId AND BuildingId = @BuildingId
        """
    |> Sql.parameters [ 
        "@OrganizationId", Sql.uuid orgId 
        "@BuildingId", Sql.uuidOrNone buildingId
    ]
    |> Sql.writeAsync

let private createOrganizationTypeQuery =
    """
        INSERT INTO OrganizationTypes (OrganizationTypeId, Name) 
        VALUES (@OrganizationTypeId, @Name)
    """

let private updateOrganizationTypeQuery =
    """
        UPDATE OrganizationTypes 
        SET Name = @Name 
        WHERE OrganizationTypeId = @OrganizationTypeId
    """

let private paramsForOrganizationType (validated: ValidatedOrganizationType) = [
    "@OrganizationTypeId", Sql.uuid validated.OrganizationTypeId
    "@Name"              , Sql.string (string validated.Name)
]
    
let createOrganizationType (connectionString) (validated: ValidatedOrganizationType) =
    Sql.connect connectionString
    |> Sql.query createOrganizationTypeQuery
    |> Sql.parameters (paramsForOrganizationType validated)
    |> Sql.writeAsync
    |> Async.map (fun _ -> Query.clearCache(); ())

let updateOrganizationType (connectionString) (validated: ValidatedOrganizationType) =
    Sql.connect connectionString
    |> Sql.query updateOrganizationTypeQuery
    |> Sql.parameters (paramsForOrganizationType validated)
    |> Sql.writeAsync
    |> Async.map (fun r -> Query.clearCache(); r)

let deleteOrganizationType (connectionString) (orgTypeId: Guid) =
    Sql.connect connectionString
    |> Sql.writeBatchAsync [
        "DELETE FROM OrganizationOrganizationTypeLinks WHERE OrganizationTypeId = @OrganizationTypeId",
            [ "@OrganizationTypeId", Sql.uuid orgTypeId ]
        "DELETE FROM OrganizationTypes WHERE OrganizationTypeId = @OrganizationTypeId",
            [ "@OrganizationTypeId", Sql.uuid orgTypeId ]
    ]
    |> Async.map (fun r -> Query.clearCache(); r |> List.skip 1 |> List.tryHead |> Option.defaultValue 0)

let makeStorage conn = {
    new IOrganizationStorage with
        member _.CreateContactPerson cp = createContactPerson conn cp
        member _.UpdateContactPerson cp = updateContactPerson conn cp
        member _.DeleteContactPerson (buildingId, cpId) = deleteContactPerson conn (buildingId, cpId)
        member _.CreateOrganization org = createOrganization conn org
        member _.UpdateOrganization org = updateOrganization conn org
        member _.DeleteOrganization (buildingId, orgId) = deleteOrganization conn (buildingId, orgId)
        member _.CreateOrganizationType orgType = createOrganizationType conn orgType
        member _.UpdateOrganizationType orgType = updateOrganizationType conn orgType
        member _.DeleteOrganizationType orgTypeId = deleteOrganizationType conn orgTypeId
}