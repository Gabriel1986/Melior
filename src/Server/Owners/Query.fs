module Server.Owners.Query

open System
open Shared.Domain
open Server.Library

let dummyOwners: Owner list = [
    {
        BuildingId = Guid.NewGuid()
        Person = {
            PersonId = Guid.NewGuid()
            FirstName = Some "Jan"
            LastName = Some "Janssens"
            Title = Some "Mr."
            LanguageCode = Some "nl-BE"
            Gender = Gender.Male
            MainAddress = {
                Street = "Teststraat 100 bus 101"
                ZipCode = "0001"
                Town = "Testgemeente"
                Country = "Belgium"
            }
            ContactAddress = MainAddress
            OtherAddresses = []
            MainTelephoneNumber = Some "555-123-123"
            MainTelephoneNumberComment = None
            MainEmailAddress = Some "JanJanssens@test.be"
            MainEmailAddressComment = None
            OtherContactMethods = []
        }
        IsResident = false
        IsActive = true
    }
    {
        BuildingId = Guid.NewGuid()
        Person = {
            PersonId = Guid.NewGuid()
            FirstName = Some "Jefke"
            LastName = Some "Janssens"
            Title = Some "Dr."
            LanguageCode = Some "en-US"
            Gender = Gender.Male
            MainAddress = {
                Street = "Teststraat 100 bus 101"
                ZipCode = "0001"
                Town = "Testgemeente"
                Country = "Belgium"
            }
            ContactAddress = MainAddress
            OtherAddresses = []
            MainTelephoneNumber = Some "555-123-123"
            MainTelephoneNumberComment = None
            MainEmailAddress = Some "DrJefkeJanssens@test.be"
            MainEmailAddressComment = None
            OtherContactMethods = []
        }
        IsResident = true
        IsActive = true
    }
    {
        BuildingId = Guid.NewGuid()
        Person = {
            PersonId = Guid.NewGuid()
            FirstName = Some "Jeanneke"
            LastName = Some "Janssens"
            Title = Some "Mevr."
            LanguageCode = Some "nl-BE"
            Gender = Gender.Female
            MainAddress = {
                Street = "Teststraat 100 bus 102"
                ZipCode = "0001"
                Town = "Testgemeente"
                Country = "Belgium"
            }
            ContactAddress = ContactAddress.ContactAddress {  
                Street = "Teststraat 102"
                ZipCode = "0001"
                Town = "Testgemeente"
                Country = "Belgium"

            }
            OtherAddresses = []
            MainTelephoneNumber = Some "555-123-123"
            MainTelephoneNumberComment = None
            MainEmailAddress = Some "JeannekeJanssens@test.be"
            MainEmailAddressComment = None
            OtherContactMethods = []
        }
        IsResident = true
        IsActive = true
    }
    {
        BuildingId = Guid.NewGuid()
        Person = {
            PersonId = Guid.NewGuid()
            FirstName = Some "Flarry"
            LastName = Some "Janssens"
            Title = Some "Heli."
            LanguageCode = Some "nl-BE"
            Gender = Gender.Other
            MainAddress = {
                Street = "Teststraat 100 bus 201"
                ZipCode = "0001"
                Town = "Testgemeente"
                Country = "Belgium"
            }
            ContactAddress = MainAddress
            OtherAddresses = []
            MainTelephoneNumber = Some "555-123-123"
            MainTelephoneNumberComment = None
            MainEmailAddress = Some "FlarryJanssens@test.be"
            MainEmailAddressComment = None
            OtherContactMethods = []
        }
        IsResident = true
        IsActive = false
    }    
]

let private toListItem (owner: Owner): OwnerListItem = {
    PersonId = owner.Person.PersonId
    BuildingId = owner.BuildingId
    FirstName = owner.Person.FirstName
    LastName = owner.Person.LastName
    IsResident = owner.IsResident
    IsActive = owner.IsActive
}

let getOwners (connectionString: string) (filter: {| BuildingId: Guid |}) = async {
    return dummyOwners |> List.map toListItem
}

let getOwner (connectionString: string) (ownerId: Guid) = async {
    return dummyOwners |> List.tryFind (fun owner -> owner.Person.PersonId = ownerId)
}