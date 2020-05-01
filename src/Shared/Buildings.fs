module Shared.Buildings

open System
open Shared.Domain

type SyndicId =
    | ResidentId of Guid
    | ProfessionalSyndicId of Guid
    | Other of Person

type ConciergeId =
    | ResidentId of Guid
    | NonResident of Person

let private mapSyndicToSyndicId =
    function
    | Syndic.ProfessionalSyndic prof -> 
        SyndicId.ProfessionalSyndicId prof.ProfessionalSyndicId
    | Syndic.Resident resident ->
        SyndicId.ResidentId resident.ResidentId
    | Syndic.Other person ->
        SyndicId.Other person

let private mapConciergeToConciergeId =
    function
    | Concierge.Resident resident ->
        ConciergeId.ResidentId resident.ResidentId
    | Concierge.NonResident person ->
        ConciergeId.NonResident person

type CreateBuildingRequest = 
    {
        BuildingId: Guid
        Code: string
        Name: string
        Address: Address
        OrganizationNumber: string option
        Remarks: string option
        GeneralMeetingFrom: DateTimeOffset option
        GeneralMeetingUntil: DateTimeOffset option
        Syndic: SyndicId option
        Concierge: ConciergeId option
    }
    static member From (building: Building): CreateBuildingRequest = {
        BuildingId = building.BuildingId
        Code = building.Code
        Name = building.Name
        Address = building.Address
        OrganizationNumber = building.OrganizationNumber
        Remarks = building.Remarks
        GeneralMeetingFrom = building.GeneralMeetingFrom
        GeneralMeetingUntil = building.GeneralMeetingUntil
        Syndic = building.Syndic |> Option.map mapSyndicToSyndicId
        Concierge = building.Concierge |> Option.map mapConciergeToConciergeId
    }


type UpdateBuildingRequest = 
    {
        BuildingId: Guid
        Name: string
        Address: Address
        OrganizationNumber: string option
        Remarks: string option
        GeneralMeetingFrom: DateTimeOffset option
        GeneralMeetingUntil: DateTimeOffset option
        Syndic: SyndicId option
        Concierge: ConciergeId option
    }
    static member From (building: Building): UpdateBuildingRequest = {
        BuildingId = building.BuildingId
        Name = building.Name
        Address = building.Address
        OrganizationNumber = building.OrganizationNumber
        Remarks = building.Remarks
        GeneralMeetingFrom = building.GeneralMeetingFrom
        GeneralMeetingUntil = building.GeneralMeetingUntil
        Syndic = building.Syndic |> Option.map mapSyndicToSyndicId
        Concierge = building.Concierge |> Option.map mapConciergeToConciergeId
    }