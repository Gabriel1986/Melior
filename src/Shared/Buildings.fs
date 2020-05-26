module Shared.Buildings

open System
open Shared.Domain
open Shared.ConstrainedTypes

type SyndicId =
    | OwnerId of Guid
    | ProfessionalSyndicId of Guid
    | OtherId of Guid

type ConciergeId =
    | OwnerId of Guid
    | NonOwnerId of Guid

let private mapSyndicToSyndicId =
    function
    | Syndic.ProfessionalSyndic prof -> 
        SyndicId.ProfessionalSyndicId prof.ProfessionalSyndicId
    | Syndic.Owner owner ->
        SyndicId.OwnerId owner.Person.PersonId
    | Syndic.Other person ->
        SyndicId.OtherId person.PersonId

let private mapConciergeToConciergeId =
    function
    | Concierge.Owner owner ->
        ConciergeId.OwnerId owner.Person.PersonId
    | Concierge.NonOwner person ->
        ConciergeId.NonOwnerId person.PersonId

type BuildingRequest = 
    {
        BuildingId: Guid
        Code: string
        Name: string
        Address: Address
        OrganizationNumber: OrganizationNumber option
        Remarks: string option
        GeneralMeetingPeriod: GeneralMeetingPeriod option
        SyndicId: SyndicId option
        ConciergeId: ConciergeId option
        YearOfConstruction: int option
        YearOfDelivery: int option
    }
    static member From (building: Building): BuildingRequest = {
        BuildingId = building.BuildingId
        Code = building.Code
        Name = building.Name
        Address = building.Address
        OrganizationNumber = building.OrganizationNumber
        Remarks = building.Remarks
        GeneralMeetingPeriod = building.GeneralMeetingPeriod
        SyndicId = building.Syndic |> Option.map mapSyndicToSyndicId
        ConciergeId = building.Concierge |> Option.map mapConciergeToConciergeId
        YearOfConstruction = building.YearOfConstruction
        YearOfDelivery = building.YearOfDelivery
    }