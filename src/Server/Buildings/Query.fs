namespace Server.Buildings

open System
open Shared.Domain
open Server.Library

module Query =
    let dummyBuildings: Building list = [
        {
            BuildingId = Guid.NewGuid()
            IsActive = true
            Code = "BUILD01"
            Name = "Minimaal gebouw"
            Address = {
                Street = "Teststraat"
                ZipCode = "0001"
                Town = "Test"
            }
            OrganizationNumber = None
            Remarks = None
            GeneralMeetingFrom = None
            GeneralMeetingUntil = None
            Concierge = None
            Syndic = None
        }
        {
            BuildingId = Guid.NewGuid()
            IsActive = true
            Code = "BUILD02"
            Name = "Gebouw met alles ingevuld"
            Address = {
                Street = "Teststraat"
                ZipCode = "0001"
                Town = "Test"
            }
            OrganizationNumber = Some "8886-555-44"
            Remarks = Some "Dit is een opmerking, er zijn er vele, maar dit is de mijne."
            GeneralMeetingFrom = Some (DateTimeOffset.Now.AddDays(20.0))
            GeneralMeetingUntil = Some (DateTimeOffset.Now.AddDays(50.0))
            Concierge = Some (Concierge.NonResident {
                    PersonId = Guid.NewGuid()
                    FirstName = "Joske"
                    LastName = "Vermeulen"
                    LetterPreamble = "Mr."
                    MainAddress = {
                        Street = "Trammezandlei 122"
                        ZipCode = "2900"
                        Town = "Schoten"
                    }
                    Language = {
                        LanguageId = Guid.NewGuid()
                        Name = "Nederlands"
                        Code = "nl-be"
                    }
                    Gender = Gender.Male
                    OtherAddresses = []
                }
            )
            Syndic = Some (Syndic.Other {
                    PersonId = Guid.NewGuid()
                    FirstName = "Jeanneke"
                    LastName = "Vermeulen"
                    LetterPreamble = "Mr."
                    MainAddress = {
                        Street = "Trammezandlei 122"
                        ZipCode = "2900"
                        Town = "Schoten"
                    }
                    Language = {
                        LanguageId = Guid.NewGuid()
                        Name = "Nederlands"
                        Code = "nl-be"
                    }
                    Gender = Gender.Female
                    OtherAddresses = []
                }
            )
        }
    ]
    let buildingToListItem (building: Building): BuildingListItem = {
        BuildingId = building.BuildingId
        IsActive = building.IsActive
        Code = building.Code
        Name = building.Name
        Address = building.Address
        OrganizationNumber = building.OrganizationNumber
    }

    let getBuilding connectionString (msg: Message<Guid>) = async {
        return dummyBuildings |> List.tryFind (fun building -> building.BuildingId = msg.Payload)            
    }

    let getBuildings connectionString (msg: Message<unit>) = async {
        return dummyBuildings |> List.map (fun building -> buildingToListItem building)
    }