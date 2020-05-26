module Server.Buildings.Query

open System
open Shared.Domain
open Server.Library
open Npgsql.FSharp
open Server.Addresses.Library
open Shared.ConstrainedTypes
open Server.PostgreSQL
open Serilog
open Server.PostgreSQL.Sql

[<AutoOpen>]
module private Readers =
    type BuildingDbModel = {
        BuildingId: Guid
        IsActive: bool
        Code: string
        Name: string
        Address: string
        OrganizationNumber: string option
        Remarks: string option
        GeneralMeetingFrom: DateTime option
        GeneralMeetingUntil: DateTime option
        ConciergePersonId: Guid option
        ConciergeOwnerId: Guid option
        SyndicOwnerId: Guid option
        SyndicProfessionalSyndicId: Guid option
        SyndicPersonId: Guid option
        YearOfConstruction: int option
        YearOfDelivery: int option
    }

    let private read (f: RowReader -> string -> 'T) (str: string) (reader: RowReader): 'T =
        (f reader (str.ToLower()))

    let private readString (str: string) (reader: RowReader) =
        reader.string (str.ToLower())

    let private readBool (str: string) (reader: RowReader) =
        reader.bool (str.ToLower())

    let readBuilding (reader: RowReader): BuildingDbModel = 
        let reader = new CaseInsensitiveRowReader(reader)
        {
            BuildingId = reader.uuid "BuildingId"
            IsActive = reader.bool "IsActive"
            Code = reader.string "Code"
            Name = reader.string "Name"
            Address = reader.string "Address"
            OrganizationNumber = reader.stringOrNone "OrganizationNumber"
            Remarks = reader.stringOrNone "Remarks"
            GeneralMeetingFrom = reader.dateTimeOrNone "GeneralMeetingFrom"
            GeneralMeetingUntil = reader.dateTimeOrNone "GeneralMeetingUntil"
            ConciergePersonId = reader.uuidOrNone "ConciergePersonId"
            ConciergeOwnerId = reader.uuidOrNone "ConciergeOwnerId"
            SyndicOwnerId = reader.uuidOrNone "SyndicOwnerId"
            SyndicProfessionalSyndicId = reader.uuidOrNone "SyndicProfessionalSyndicId"
            SyndicPersonId = reader.uuidOrNone "SyndicPersonId"
            YearOfConstruction = reader.intOrNone "YearOfConstruction"
            YearOfDelivery = reader.intOrNone "YearOfDelivery"
        }

    type BuildingListItemDbModel = {
        BuildingId: Guid
        IsActive: bool
        Code: string
        Name: string
        Address: string
        OrganizationNumber: string option
    }

    let readBuildingListItem (reader: RowReader): BuildingListItemDbModel = 
        let reader = new CaseInsensitiveRowReader(reader)
        {
            BuildingId = reader.uuid "BuildingId"
            IsActive = reader.bool "IsActive"
            Code = reader.string "Code"
            Name = reader.string "Name"
            Address = reader.string "Address"
            OrganizationNumber = reader.stringOrNone "OrganizationNumber"
        }

let private forceAddress str =
    match Address.fromJson str with
    | Ok addr -> addr
    | Error _ -> Address.Init

let getBuilding connectionString (buildingId: Guid): Async<Building option> = async {
    let! result =
        Sql.connect connectionString
        |> Sql.query
            """
                SELECT
                    BuildingId,
                    IsActive,
                    Code,
                    Name,
                    Address,
                    OrganizationNumber,
                    Remarks,
                    GeneralMeetingFrom,
                    GeneralMeetingUntil,
                    ConciergePersonId,
                    ConciergeOwnerId,
                    SyndicOwnerId,
                    SyndicProfessionalSyndicId,
                    SyndicPersonId,
                    YearOfConstruction,
                    YearOfDelivery
                FROM Buildings
                WHERE BuildingId = @BuildingId
            """
        |> Sql.parameters [ "@BuildingId", Sql.uuid buildingId ]
        |> Sql.readSingle readBuilding

    match result with
    | Some dbModel ->
        let! concierge =
            match dbModel.ConciergeOwnerId, dbModel.ConciergePersonId with
            | Some ownerId, _ -> 
                Server.Owners.Query.getOwner connectionString ownerId
                |> Async.map (Option.map Concierge.Owner)
            | _, Some personId -> 
                Server.Persons.Query.getPerson connectionString personId
                |> Async.map (Option.map Concierge.NonOwner)
            | _ ->
                Async.lift None

        let! syndic =
            match dbModel.SyndicOwnerId, dbModel.SyndicPersonId, dbModel.SyndicProfessionalSyndicId with
            | Some ownerId, _, _ ->
                Server.Owners.Query.getOwner connectionString ownerId
                |> Async.map (Option.map Syndic.Owner)
            | _, Some personId, _ ->
                Server.Persons.Query.getPerson connectionString personId
                |> Async.map (Option.map Syndic.Other)
            | _, _, Some proSyndicId -> 
                Server.ProfessionalSyndics.Query.getProfessionalSyndic connectionString proSyndicId
                |> Async.map (Option.map Syndic.ProfessionalSyndic)
            | _ ->
                Async.lift None

        return Some {
            BuildingId = dbModel.BuildingId
            IsActive = dbModel.IsActive
            Code = dbModel.Code
            Name = dbModel.Name
            Address = dbModel.Address |> forceAddress                
            OrganizationNumber = 
                dbModel.OrganizationNumber
                |> Option.bind OrganizationNumber.OfString
            Remarks = dbModel.Remarks
            GeneralMeetingPeriod =
                match dbModel.GeneralMeetingFrom, dbModel.GeneralMeetingUntil with
                | Some from, Some until -> Some { FromDay = from.Day; FromMonth = from.Month; UntilDay = until.Day; UntilMonth = until.Month }
                | _ -> None
            Concierge = concierge
            Syndic = syndic
            YearOfConstruction = dbModel.YearOfConstruction
            YearOfDelivery = dbModel.YearOfDelivery
        }
    | None ->
        return None
}



let getBuildings connectionString (): Async<BuildingListItem list> = async {
    let! results =
        Sql.connect connectionString
        |> Sql.query
            """
                SELECT
                    BuildingId,
                    IsActive,
                    Code,
                    Name,
                    Address,
                    OrganizationNumber
                FROM Buildings
            """
        |> Sql.parameters []
        |> Sql.read readBuildingListItem

    return results |> List.map (fun dbModel -> {
        BuildingId = dbModel.BuildingId
        IsActive = dbModel.IsActive
        Code = dbModel.Code
        Name = dbModel.Name
        Address = dbModel.Address |> forceAddress
        OrganizationNumber = 
            dbModel.OrganizationNumber
            |> Option.bind OrganizationNumber.OfString
    })
}