module Server.Remoting
    open System
    open Microsoft.AspNetCore.Http

    open Shared.Remoting
    open Server.Library
    open Server.Blueprint.Behavior
    open LibraryExtensions

    let meliorBff (environment: IEnv) (ctx: HttpContext): RemotingApi = 
        let createMsg payload: Message<'T> = {
            CreatedAt = DateTimeOffset.Now
            Context = ctx
            Payload = payload
        } 

        {
            GetCurrentUser = fun _ ->
                User.OfContext ctx 
                |> Async.lift
            CreateBuilding = fun building -> 
                createMsg building
                |> environment.BuildingSystem.CreateBuilding
            UpdateBuilding = fun building ->
                createMsg building
                |> environment.BuildingSystem.UpdateBuilding
            DeleteBuilding = fun buildingId ->
                createMsg buildingId
                |> environment.BuildingSystem.DeleteBuilding
            UpdateBuildingSyndic = fun (buildingId, syndicId) ->
                createMsg (buildingId, syndicId)
                |> environment.BuildingSystem.UpdateBuildingSyndic
            UpdateBuildingConcierge = fun (buildingId, conciergeId) ->
                createMsg (buildingId, conciergeId)
                |> environment.BuildingSystem.UpdateBuildingConcierge
            GetBuilding = fun buildingId ->
                createMsg buildingId
                |> environment.BuildingSystem.GetBuilding
            GetBuildings = fun arg ->
                createMsg arg
                |> environment.BuildingSystem.GetBuildings
            CreateOwner = fun owner ->
                createMsg owner
                |> environment.OwnerSystem.CreateOwner
            UpdateOwner = fun owner ->
                createMsg owner
                |> environment.OwnerSystem.UpdateOwner
            DeleteOwner = fun ownerId ->
                createMsg ownerId
                |> environment.OwnerSystem.DeleteOwner
            GetOwners = fun buildingFilter ->
                createMsg {| BuildingId = buildingFilter.BuildingId |}
                |> environment.OwnerSystem.GetOwners            
            GetOwner = fun ownerId ->
                createMsg ownerId
                |> environment.OwnerSystem.GetOwner
            CreateLot = fun lot ->
                createMsg lot
                |> environment.LotSystem.CreateLot
            UpdateLot = fun lot ->
                createMsg lot
                |> environment.LotSystem.UpdateLot
            DeleteLot = fun lotId ->
                createMsg lotId
                |> environment.LotSystem.DeleteLot
            GetLots = fun buildingFilter ->
                createMsg {| BuildingId = buildingFilter.BuildingId |}
                |> environment.LotSystem.GetLots
            GetLot = fun lotId ->
                createMsg lotId
                |> environment.LotSystem.GetLot
            CreateOrganization = fun org ->
                createMsg org
                |> environment.OrganizationSystem.CreateOrganization
            UpdateOrganization = fun org ->
                createMsg org
                |> environment.OrganizationSystem.UpdateOrganization
            DeleteOrganization = fun orgNr ->
                createMsg orgNr
                |> environment.OrganizationSystem.DeleteOrganization
            GetOrganizations = fun buildingFilter ->
                createMsg {| BuildingId = buildingFilter.BuildingId |}
                |> environment.OrganizationSystem.GetOrganizations
            GetOrganization = fun orgNr ->
                createMsg orgNr
                |> environment.OrganizationSystem.GetOrganization
            VerifyVatNumber = fun vatNumber ->
                environment.OrganizationSystem.VerifyVatNumber vatNumber
            CreateProfessionalSyndic = fun syndic ->
                createMsg syndic
                |> environment.ProfessionalSyndicSystem.CreateProfessionalSyndic
            UpdateProfessionalSyndic = fun syndic ->
                createMsg syndic
                |> environment.ProfessionalSyndicSystem.UpdateProfessionalSyndic
            DeleteProfessionalSyndic = fun syndicId ->
                createMsg syndicId
                |> environment.ProfessionalSyndicSystem.DeleteProfessionalSyndic
            GetProfessionalSyndics = fun arg ->
                createMsg arg
                |> environment.ProfessionalSyndicSystem.GetProfessionalSyndics
            GetProfessionalSyndic = fun syndicId ->
                createMsg syndicId
                |> environment.ProfessionalSyndicSystem.GetProfessionalSyndic
            GetOrganizationTypes = fun arg ->
                arg
                |> environment.OrganizationSystem.GetOrganizationTypes
            CreateOrganizationType = fun orgType ->
                createMsg orgType
                |> environment.OrganizationSystem.CreateOrganizationType
            UpdateOrganizationType = fun orgType ->
                createMsg orgType
                |> environment.OrganizationSystem.UpdateOrganizationType
            DeleteOrganizationType = fun orgTypeId ->
                createMsg orgTypeId
                |> environment.OrganizationSystem.DeleteOrganizationType

            GetContractTypeAnswers = fun filter ->
                createMsg {| BuildingId = filter.BuildingId |}
                |> environment.ContractSystem.GetContractTypeAnswers
            SaveContractTypeAnswer = fun arg ->
                createMsg arg
                |> environment.ContractSystem.SaveContractTypeAnswer
            GetContracts = fun filter ->
                createMsg {| BuildingId = filter.BuildingId |}
                |> environment.ContractSystem.GetContracts
            CreateContract = fun arg ->
                createMsg arg
                |> environment.ContractSystem.CreateContract
            UpdateContract = fun arg ->
                createMsg arg
                |> environment.ContractSystem.UpdateContract
            DeleteContract = fun (buildingId, contractId) ->
                createMsg (buildingId, contractId)
                |> environment.ContractSystem.DeleteContract
        }