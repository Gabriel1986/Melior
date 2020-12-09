module Server.StorageEngine.PersonStorage

open Npgsql.FSharp
open Shared.Write
open Server.Library
open Server.PostgreSQL
open Server.Blueprint.Data.Storage
open Server.Addresses.Library
open Server.ContactMethods.Workflow
open Server.Blueprint.Data.Financial

let private paramsFor (validated: ValidatedPerson) = [
    "@PersonId"                  , Sql.uuid validated.PersonId
    "@FirstName"                 , Sql.stringOrNone (validated.FirstName |> Option.map string)
    "@LastName"                  , Sql.stringOrNone (validated.LastName |> Option.map string)
    "@LanguageCode"              , Sql.stringOrNone (validated.LanguageCode |> Option.map string)
    "@Gender"                    , Sql.string (string validated.Gender)
    "@Title"                     , Sql.stringOrNone (validated.Title |> Option.map string)
    "@MainAddress"               , Sql.jsonb (validated.MainAddress |> ValidatedAddress.toJson)
    "@ContactAddress"            , Sql.jsonbOrNone (validated.ContactAddress |> Option.map ValidatedAddress.toJson)
    "@OtherAddresses"            , Sql.jsonb (validated.OtherAddresses |> ValidatedOtherAddress.listToJson)
    "@MainTelephoneNumber"       , Sql.stringOrNone (validated.MainTelephoneNumber |> Option.map string)
    "@MainTelephoneNumberComment", Sql.stringOrNone (validated.MainTelephoneNumberComment |> Option.map string)
    "@MainEmailAddress"          , Sql.stringOrNone (validated.MainEmailAddress |> Option.map string)
    "@MainEmailAddressComment"   , Sql.stringOrNone (validated.MainEmailAddressComment|> Option.map string)
    "@OtherContactMethods"       , Sql.jsonb (validated.OtherContactMethods |> ValidatedContactMethod.listToJson)
    "@BankAccounts"              , Sql.jsonb (validated.BankAccounts |> ValidatedBankAccount.listToJson)
]

let [<Literal>] private createQuery =
    """
        INSERT INTO Persons (
            PersonId,
            FirstName,
            LastName,
            LanguageCode,
            Gender,
            Title,
            MainAddress,
            ContactAddress,
            OtherAddresses,
            MainTelephoneNumber,
            MainTelephoneNumberComment,
            MainEmailAddress,
            MainEmailAddressComment,
            OtherContactMethods,
            BankAccounts
        ) VALUES (
            @PersonId,
            @FirstName,
            @LastName,
            @LanguageCode,
            @Gender,
            @Title,
            @MainAddress,
            @ContactAddress,
            @OtherAddresses,
            @MainTelephoneNumber,
            @MainTelephoneNumberComment,
            @MainEmailAddress,
            @MainEmailAddressComment,
            @OtherContactMethods,
            @BankAccounts
        )
    """

let [<Literal>] private updateQuery =
    """
        UPDATE Persons
        SET 
            FirstName = @FirstName, 
            LastName = @LastName, 
            LanguageCode = @LanguageCode,
            Gender = @Gender,
            Title = @Title,
            MainAddress = @MainAddress,
            ContactAddress = @ContactAddress,
            OtherAddresses = @OtherAddresses,
            MainTelephoneNumber = @MainTelephoneNumber,
            MainTelephoneNumberComment = @MainTelephoneNumberComment,
            MainEmailAddress = @MainEmailAddress,
            MainEmailAddressComment = @MainEmailAddressComment,
            OtherContactMethods = @OtherContactMethods,
            BankAccounts = @BankAccounts
        WHERE PersonId = @PersonId
    """

let private upsertQuery =
    sprintf
        """
            %s
            ON CONFLICT ON CONSTRAINT persons_pkey DO UPDATE
            %s
        """
        createQuery
        (updateQuery.[updateQuery.IndexOf("SET")..(updateQuery.IndexOf("WHERE") - 1)])


let transformEventToSql (msg: Message<PersonEvent>) =
    match msg.Payload with
    | PersonEvent.PersonEvent event ->
        match event with
        | CUDEvent.Created validated ->
            [ createQuery, [ paramsFor validated ] ]
        | CUDEvent.Updated validated ->
            [ updateQuery, [ paramsFor validated ] ]
        | CUDEvent.Deleted personId ->
            [
                "UPDATE Persons SET IsDeleted = TRUE WHERE PersonId = @PersonId"
                , [[ "@PersonId", Sql.uuid personId ]]
            ]
    | PersonEvent.PersonWasSaved validated ->
        [ upsertQuery, [ paramsFor validated ] ]