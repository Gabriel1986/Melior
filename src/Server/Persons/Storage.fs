module Server.Persons.Storage

open Npgsql.FSharp

open Shared.Read
open Shared.Write
open Server.Library
open Server.PostgreSQL
open Server.Addresses.Workflow
open Server.ContactMethods.Workflow
open Server.Persons.Library

module ValidatedOtherAddress =
    let toOtherAddress (validated: ValidatedOtherAddress): OtherAddress = {
        Name = string validated.Name
        Description = validated.Description
        Address = validated.Address |> ValidatedAddress.toAddress
    }

    let toJson (validated: ValidatedOtherAddress): string =
        validated |> toOtherAddress |> OtherAddress.toJson

    let listToJson (validated: ValidatedOtherAddress list): string =
        validated |> List.map toOtherAddress |> OtherAddress.listToJson

[<NoComparison; NoEquality>]
type IPersonStorage =
    abstract CreatePerson: ValidatedPerson -> Async<unit>
    abstract UpdatePerson: ValidatedPerson -> Async<int>

let paramsFor (validated: ValidatedPerson) =
    [
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
        "@MainEmailAddressComment"   , Sql.stringOrNone (validated.MainTelephoneNumberComment |> Option.map string)
        "@OtherContactMethods"       , Sql.jsonb (validated.OtherContactMethods |> ValidatedContactMethod.listToJson)
    ]

let [<Literal>] createQuery =
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
            OtherContactMethods
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
            @OtherContactMethods
        )
    """

let [<Literal>] updateQuery =
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
            OtherContactMethods = @OtherContactMethods
        WHERE PersonId = @PersonId
    """

let upsertQuery =
    sprintf
        """
            %s
            ON CONFLICT (persons_pkey) DO UPDATE
            %s
        """
        createQuery
        (updateQuery.[updateQuery.IndexOf("SET")..])

let createPerson (connectionString: string) (validated: ValidatedPerson) =
    Sql.connect connectionString
    |> Sql.query createQuery
    |> Sql.parameters (paramsFor validated)
    |> Sql.writeAsync
    |> Async.Ignore

let updatePerson connectionString validated =
    Sql.connect connectionString
    |> Sql.query updateQuery
    |> Sql.parameters (paramsFor validated)
    |> Sql.writeAsync

let makeStorage conn = {
    new IPersonStorage with
        member _.CreatePerson person = createPerson conn person 
        member _.UpdatePerson person = updatePerson conn person
}