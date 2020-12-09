module Server.StorageEngine.OrganizationStorage

open System
open Npgsql.FSharp
open Shared.Write
open Shared.Read
open Server.Library
open Server.PostgreSQL
open Server.Blueprint.Data.Storage
open Server.Addresses.Library
open Server.ContactMethods.Workflow
open Server.Blueprint.Data.Financial

let private paramsFor (validated: ValidatedOrganization) = [
    "@OrganizationId"            , Sql.uuid validated.OrganizationId
    "@BuildingId"                , Sql.uuidOrNone validated.BuildingId
    "@UsesVatNumber"             , Sql.bool validated.UsesVatNumber
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
    "@BankAccounts"              , Sql.jsonb (validated.BankAccounts |> ValidatedBankAccount.listToJson)
]

let private paramsForOrganizationType (validated: ValidatedOrganizationType) = [
    "@OrganizationTypeId", Sql.uuid validated.OrganizationTypeId
    "@Name", Sql.string (string validated.Name)
]

let private paramsForContactPerson (validated: ValidatedContactPerson) = [
    "@PersonId"              , Sql.uuid   validated.Person.PersonId
    "@OrganizationId"        , Sql.uuid   validated.OrganizationId
    "@RoleWithinOrganization", Sql.string (string validated.RoleWithinOrganization)
]

let private setOrganizationTypeLinks (validated: ValidatedOrganization) =
    [
        """
            Delete FROM OrganizationOrganizationTypeLinks WHERE OrganizationId = @OrganizationId
        """
        , [[ "@OrganizationId", Sql.uuid validated.OrganizationId ]]

        """
            INSERT INTO OrganizationOrganizationTypeLinks (OrganizationId, OrganizationTypeId) 
            VALUES (@OrganizationId, @OrganizationTypeId)
        """, validated.OrganizationTypeIds |> List.map (fun otId -> [
            "@OrganizationId", Sql.uuid validated.OrganizationId
            "@OrganizationTypeId", Sql.uuid otId 
        ])
    ]

let transformEventToSql (msg: Message<OrganizationEvent>) =
    match msg.Payload with
    | OrganizationEvent.OrganizationWasCreated validated ->
        [
            """
                INSERT INTO Organizations (
                    OrganizationId,
                    BuildingId,
                    UsesVatNumber,
                    OrganizationNumber,
                    VatNumber,
                    VatNumberVerifiedOn,
                    Name,
                    Address,
                    MainTelephoneNumber,
                    MainTelephoneNumberComment,
                    MainEmailAddress,
                    MainEmailAddressComment,
                    OtherContactMethods,
                    BankAccounts
                ) VALUES (
                    @OrganizationId,
                    @BuildingId,
                    @UsesVatNumber,
                    @OrganizationNumber,
                    @VatNumber,
                    @VatNumberVerifiedOn,
                    @Name,
                    @Address,
                    @MainTelephoneNumber,
                    @MainTelephoneNumberComment,
                    @MainEmailAddress,
                    @MainEmailAddressComment,
                    @OtherContactMethods,
                    @BankAccounts
                )
            """, [ paramsFor validated ]
        ] @ setOrganizationTypeLinks validated
    | OrganizationEvent.OrganizationWasUpdated validated ->
        [
            """
                UPDATE Organizations SET
                    OrganizationNumber = @OrganizationNumber,
                    UsesVatNumber = @UsesVatNumber,
                    VatNumber = @VatNumber,
                    VatNumberVerifiedOn = @VatNumberVerifiedOn,
                    Name = @Name,
                    Address = @Address,
                    MainTelephoneNumber = @MainTelephoneNumber,
                    MainTelephoneNumberComment = @MainTelephoneNumberComment,
                    MainEmailAddress = @MainEmailAddress,
                    MainEmailAddressComment = @MainEmailAddressComment,
                    OtherContactMethods = @OtherContactMethods,
                    BankAccounts = @BankAccounts
                WHERE OrganizationId = @OrganizationId
            """, [ paramsFor validated ]
        ] @ setOrganizationTypeLinks validated
    | OrganizationEvent.OrganizationWasDeleted (buildingId: BuildingId option, organizationId: Guid) ->
        [
            """
                UPDATE Organizations 
                SET IsActive = FALSE
                WHERE OrganizationId = @OrganizationId AND BuildingId = @BuildingId
            """, [[
                "@OrganizationId", Sql.uuid organizationId 
                "@BuildingId", Sql.uuidOrNone buildingId
            ]]
        ]
    | OrganizationEvent.OrganizationTypeEvent event ->
        match event with
        | CUDEvent.Created validated ->
            [
                "INSERT INTO OrganizationTypes (OrganizationTypeId, Name)  VALUES (@OrganizationTypeId, @Name)"
                , [ paramsForOrganizationType validated ]
            ]
        | CUDEvent.Updated validated ->
            [
                """
                    UPDATE OrganizationTypes 
                    SET Name = @Name 
                    WHERE OrganizationTypeId = @OrganizationTypeId
                """
                , [ paramsForOrganizationType validated ]
            ]
        | CUDEvent.Deleted organizationTypeId ->
            [
                "DELETE FROM OrganizationOrganizationTypeLinks WHERE OrganizationTypeId = @OrganizationTypeId"
                , [[ "@OrganizationTypeId", Sql.uuid organizationTypeId ]]
                "DELETE FROM OrganizationTypes WHERE OrganizationTypeId = @OrganizationTypeId"
                , [[ "@OrganizationTypeId", Sql.uuid organizationTypeId ]]
            ]
    | OrganizationEvent.ContactPersonWasCreated validated ->
        [
            yield! PersonStorage.transformEventToSql (PersonEvent.PersonEvent (CUDEvent.Created validated.Person) |> inMsg msg)
        
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
            """, [ paramsForContactPerson validated ]

        ]
    | OrganizationEvent.ContactPersonWasUpdated validated ->
        [
            yield! PersonStorage.transformEventToSql (PersonEvent.PersonEvent (CUDEvent.Updated validated.Person) |> inMsg msg)
        
            """
                UPDATE ContactPersons 
                SET RoleWithinOrganization = @RoleWithinOrganization
                WHERE PersonId = @PersonId
            """, [ paramsForContactPerson validated ]
        ]
    | OrganizationEvent.ContactPersonWasDeleted (buildingId: BuildingId option, personId: Guid) ->
        [
            yield! PersonStorage.transformEventToSql (PersonEvent.PersonEvent (CUDEvent.Deleted personId) |> inMsg msg)

            """
                UPDATE cp
                SET IsActive = FALSE
                FROM ContactPersons cp
                LEFT JOIN Organizations o on cp.OrganizationId = o.OrganizationId
                WHERE cp.PersonId = @PersonId AND o.BuildingId = @BuildingId
            """, [[
                "@PersonId" , Sql.uuid personId
                "@BuildingId", Sql.uuidOrNone buildingId
            ]]
        ]