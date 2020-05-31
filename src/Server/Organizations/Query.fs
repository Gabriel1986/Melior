module Server.Organizations.Query

open System
open Shared
open Shared.ConstrainedTypes
open Shared.Read
open Server.Library

let dummyOrganizations: Organization list = [
    {
        OrganizationId = Guid.NewGuid()
        OrganizationNumber = OrganizationNumber.Of "OrganizationNumber" ("0888", "555", "666") |> Trial.toOption |> Option.defaultWith (failwithf "Invariant exception...")
        IsActive = true
        OrganizationType = { OrganizationTypeId = Guid.NewGuid(); Name = "Leverancier" }
        Name = ""
        Address = {
            Street = Some "Some street 33"
            ZipCode = Some "0000"
            Town = Some "Some town"
            Country = Some "Belgium"
        }
        MainContactPerson = {
            OrganizationId = Guid.NewGuid() //This is incorrect, but whatever...
            RoleWithinOrganization = "Handige Harry"
            Person = {
                PersonId = Guid.NewGuid()
                FirstName = Some "Flarry"
                LastName = Some "Janssens"
                Title = Some "Heli."
                LanguageCode = Some "nl-BE"
                Gender = Gender.Other
                MainAddress = Address.Init
                ContactAddress = MainAddress
                OtherAddresses = []
                MainTelephoneNumber = Some "555-123-123"
                MainTelephoneNumberComment = None
                MainEmailAddress = Some "FlarryJanssens@test.be"
                MainEmailAddressComment = None
                OtherContactMethods = []
            }
        }
        OtherContactPersons = []
    }  
]

let toOrganizationListItem (org: Organization) = {
    OrganizationId = org.OrganizationId
    OrganizationNumber = org.OrganizationNumber
    OrganizationType = org.OrganizationType.Name
    Name = org.Name
}

let getOrganizations (connectionString: string) (filter: {| BuildingId: Guid |})  = async {
    return dummyOrganizations |> List.map toOrganizationListItem
}

let getOrganization (connectionString: string) (orgId: Guid) = async {
    return dummyOrganizations |> List.tryFind (fun org -> org.OrganizationId = orgId)
}