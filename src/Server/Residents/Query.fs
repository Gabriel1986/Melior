namespace Server.Residents

open System
open Shared.Domain
open Server.Library

module Query =
    let dummyResidents: Resident list = [
        {
            ResidentId = Guid.NewGuid()
            Person = {
                PersonId = Guid.NewGuid()
                FirstName = "Jan"
                LastName = "Janssens"
                LetterPreamble = "Mr."
                Language = {
                    LanguageId = Guid.NewGuid()
                    Name = "Nederlands"
                    Code = "nl-be"
                }
                Gender = Gender.Male
                MainAddress = {
                    Street = "Teststraat 100 bus 101"
                    ZipCode = "0001"
                    Town = "Testgemeente"
                }
                OtherAddresses = []
            }
            BuildingId = Guid.NewGuid()
            IsActive = true
            MovedInDate = DateTimeOffset.Now.AddYears(-1)
            MovedOutDate = None
        }
        {
            ResidentId = Guid.NewGuid()
            Person = {
                PersonId = Guid.NewGuid()
                FirstName = "Jefke"
                LastName = "Janssens"
                LetterPreamble = "Dr."
                Language = {
                    LanguageId = Guid.NewGuid()
                    Name = "Nederlands"
                    Code = "nl-be"
                }
                Gender = Gender.Male
                MainAddress = {
                    Street = "Teststraat 100 bus 101"
                    ZipCode = "0001"
                    Town = "Testgemeente"
                }
                OtherAddresses = []
            }
            BuildingId = Guid.NewGuid()
            IsActive = true
            MovedInDate = DateTimeOffset.Now.AddYears(-1)
            MovedOutDate = None
        }
        {
            ResidentId = Guid.NewGuid()
            Person = {
                PersonId = Guid.NewGuid()
                FirstName = "Jeanneke"
                LastName = "Janssens"
                LetterPreamble = "Mevr."
                Language = {
                    LanguageId = Guid.NewGuid()
                    Name = "Nederlands"
                    Code = "nl-be"
                }
                Gender = Gender.Female
                MainAddress = {
                    Street = "Teststraat 100 bus 102"
                    ZipCode = "0001"
                    Town = "Testgemeente"
                }
                OtherAddresses = []
            }
            BuildingId = Guid.NewGuid()
            IsActive = true
            MovedInDate = DateTimeOffset.Now.AddYears(-1)
            MovedOutDate = None
        }
        {
            ResidentId = Guid.NewGuid()
            Person = {
                PersonId = Guid.NewGuid()
                FirstName = "Flarry"
                LastName = "Janssens"
                LetterPreamble = "Heli."
                Language = {
                    LanguageId = Guid.NewGuid()
                    Name = "Nederlands"
                    Code = "nl-be"
                }
                Gender = Gender.Other
                MainAddress = {
                    Street = "Teststraat 100 bus 201"
                    ZipCode = "0001"
                    Town = "Testgemeente"
                }
                OtherAddresses = []
            }
            BuildingId = Guid.NewGuid()
            IsActive = false
            MovedInDate = DateTimeOffset.Now.AddYears(-1)
            MovedOutDate = DateTimeOffset.Now.AddDays(-7.0) |> Some
        }    
    ]

    let private toListItem (resident: Resident): ResidentListItem = {
        ResidentId = resident.ResidentId
        BuildingId = resident.BuildingId
        FirstName = resident.Person.FirstName
        LastName = resident.Person.LastName
        IsActive = resident.IsActive
        MovedInDate = resident.MovedInDate
        MovedOutDate = resident.MovedOutDate
    }

    let getResidentsForBuilding (connectionString: string) (message: Message<Guid>) = async {
        return dummyResidents |> List.map toListItem
    }

    let getResidents (connectionString: string) (message: Message<unit>) = async {
        return dummyResidents |> List.map toListItem
    }

    let getResident (connectionString: string) (message: Message<Guid>) = async {
        return dummyResidents |> List.tryFind (fun resident -> resident.ResidentId = message.Payload)
    }