module Server.Persons.Query

open System
open Npgsql.FSharp
open Server.Library
open Server.Persons.Library
open Server.Addresses.Library
open Server.ContactMethods.Library
open Server.PostgreSQL
open Server.PostgreSQL.Sql
open Shared.Read

[<AutoOpen>]
module private Readers =
    type PersonDbModel = {
        PersonId: Guid
        FirstName: string option
        LastName: string option
        LanguageCode: string option
        Gender: string option
        Title: string option
        MainAddress: string
        ContactAddress: string option
        OtherAddresses: string option
        MainTelephoneNumber: string option
        MainTelephoneNumberComment: string option
        MainEmailAddress: string option
        MainEmailAddressComment: string option
        OtherContactMethods: string option
    }

    let readPerson (reader: CaseInsensitiveRowReader): PersonDbModel = {
        PersonId = reader.uuid "PersonId"
        FirstName = reader.stringOrNone "FirstName"
        LastName = reader.stringOrNone "LastName"
        LanguageCode = reader.stringOrNone "LanguageCode"
        Gender = reader.stringOrNone "Gender"
        Title = reader.stringOrNone "Title"
        MainAddress = reader.string "MainAddress"
        ContactAddress = reader.stringOrNone "ContactAddress"
        OtherAddresses = reader.stringOrNone "OtherAddresses"
        MainTelephoneNumber = reader.stringOrNone "MainTelephoneNumber"
        MainTelephoneNumberComment = reader.stringOrNone "MainTelephoneNumberComment"
        MainEmailAddress = reader.stringOrNone "MainEmailAddress"
        MainEmailAddressComment = reader.stringOrNone "MainEmailAddressComment"
        OtherContactMethods = reader.stringOrNone "OtherContactMethods"
    }

let private convertDbModelToDetail (dbModel: PersonDbModel): Person =
    let forceAddress =
        Address.fromJson
        >> Option.fromResult
        >> Option.defaultValue Address.Init

    let mainAddress = forceAddress dbModel.MainAddress

    let contactAddress =
        match dbModel.ContactAddress with
        | Some address -> ContactAddress.ContactAddress (forceAddress address)
        | None         -> ContactAddress.MainAddress

    let otherAddresses =
        match dbModel.OtherAddresses with
        | Some addr -> OtherAddress.listFromJson addr |> Option.fromResult |> Option.defaultValue []
        | None      -> []

    let otherContactMethods =
        match dbModel.OtherContactMethods with
        | Some str -> ContactMethod.listFromJson str |> Option.fromResult |> Option.defaultValue []
        | None     -> []

    {
        PersonId = dbModel.PersonId
        FirstName = dbModel.FirstName
        LastName = dbModel.LastName
        LanguageCode = dbModel.LanguageCode
        Gender = Gender.FromString  (defaultArg dbModel.Gender "")
        //Sir, Madame, etc.
        Title = dbModel.Title
        MainAddress = mainAddress
        ContactAddress = contactAddress
        OtherAddresses = otherAddresses
        MainTelephoneNumber = dbModel.MainTelephoneNumber
        MainTelephoneNumberComment = dbModel.MainTelephoneNumberComment
        MainEmailAddress = dbModel.MainEmailAddress
        MainEmailAddressComment = dbModel.MainEmailAddressComment
        OtherContactMethods = otherContactMethods
    }

let getPersonsByIds (connectionString: string) (personIds: Guid list) = async {
    let! result =
        Sql.connect connectionString
        |> Sql.query
            """
                SELECT
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
                FROM
                    Persons
                WHERE
                    PersonId = ANY (@PersonIds)
            """
        |> Sql.parameters [ "@PersonIds", Sql.uuidArray (personIds |> List.toArray) ]
        |> Sql.read readPerson

    return
        result |> List.map convertDbModelToDetail
}

let getPerson (connectionString: string) (personId: Guid) =
    getPersonsByIds connectionString [ personId ]
    |> Async.map List.tryHead