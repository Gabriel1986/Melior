module Server.ProfessionalSyndics.Query

open System
open Shared.Domain
open Server.Library

let dummySyndics: ProfessionalSyndic list = [
    {
        ProfessionalSyndicId = Guid.NewGuid()
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
        IsActive = true
    }
    {
        ProfessionalSyndicId = Guid.NewGuid()
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
        IsActive = true
    }
    {
        ProfessionalSyndicId = Guid.NewGuid()
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
        IsActive = true
    }
    {
        ProfessionalSyndicId = Guid.NewGuid()
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
        IsActive = false
    }    
]

let private toListItem (owner: ProfessionalSyndic): ProfessionalSyndicListItem = {
    ProfessionalSyndicId = owner.ProfessionalSyndicId
    FirstName = owner.Person.FirstName
    LastName = owner.Person.LastName
    IsActive = owner.IsActive
}

let getProfessionalSyndics (connectionString: string) () = async {
    return dummySyndics |> List.map toListItem
}

let getProfessionalSyndic (connectionString: string) (proSyndicId: Guid) = async {
    return dummySyndics |> List.tryFind (fun professionalSyndic -> professionalSyndic.ProfessionalSyndicId = proSyndicId)
}