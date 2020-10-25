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
            GetOwners = fun buildingId ->
                createMsg buildingId
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
            GetLots = fun buildingId ->
                createMsg buildingId
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
            GetOrganizations = fun buildingId ->
                createMsg buildingId
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

            GetContractTypeAnswers = fun buildingId ->
                createMsg buildingId
                |> environment.ContractSystem.GetContractTypeAnswers
            SaveContractTypeAnswer = fun arg ->
                createMsg arg
                |> environment.ContractSystem.SaveContractTypeAnswer
            GetContracts = fun buildingId ->
                createMsg buildingId
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

            GetDistributionKeys = fun buildingId ->
                createMsg buildingId
                |> environment.FinancialSystem.GetDistributionKeys
            GetDistributionKeyListItems = fun buildingId ->
                createMsg buildingId
                |> environment.FinancialSystem.GetDistributionKeyListItems
            CreateDistributionKey = fun input ->
                createMsg input
                |> environment.FinancialSystem.CreateDistributionKey
            UpdateDistributionKey = fun input ->
                createMsg input
                |> environment.FinancialSystem.UpdateDistributionKey
            DeleteDistributionKey = fun (buildingId, distributionKeyId) ->
                createMsg (buildingId, distributionKeyId)
                |> environment.FinancialSystem.DeleteDistributionKey

            GetInvoices = fun filter ->
                createMsg filter
                |> environment.FinancialSystem.GetInvoices
            GetInvoice = fun invoiceId ->
                createMsg invoiceId
                |> environment.FinancialSystem.GetInvoice
            CreateInvoice = fun input ->
                createMsg input
                |> environment.FinancialSystem.CreateInvoice
            UpdateInvoice = fun input ->
                createMsg input
                |> environment.FinancialSystem.UpdateInvoice
            DeleteInvoice = fun (buildingId, invoiceId) ->
                createMsg (buildingId, invoiceId)
                |> environment.FinancialSystem.DeleteInvoice

            GetFinancialYears = fun buildingId ->
                createMsg buildingId
                |> environment.FinancialSystem.GetFinancialYears
            CloseFinancialYear = fun (buildingId, financialYearId) ->
                createMsg (buildingId, financialYearId)
                |> environment.FinancialSystem.CloseFinancialYear
            CreateFinancialYear = fun input ->
                createMsg input
                |> environment.FinancialSystem.CreateFinancialYear
            UpdateFinancialYear = fun input ->
                createMsg input
                |> environment.FinancialSystem.UpdateFinancialYear

            GetFinancialCategories = fun buildingId ->
                createMsg buildingId
                |> environment.FinancialSystem.GetFinancialCategories
            CreateFinancialCategory = fun input ->
                createMsg input
                |> environment.FinancialSystem.CreateFinancialCategory
            UpdateFinancialCategory = fun input ->
                createMsg input
                |> environment.FinancialSystem.UpdateFinancialCategory
            DeleteFinancialCategory = fun input ->
                createMsg input
                |> environment.FinancialSystem.DeleteFinancialCategory

            GetUsers = fun input ->
                createMsg input
                |> environment.AuthenticationSystem.GetUsers
            CreateUser = fun input ->
                createMsg input
                |> environment.AuthenticationSystem.CreateUser
            UpdateUser = fun input ->
                createMsg input
                |> environment.AuthenticationSystem.UpdateUser
            DeleteUser = fun input ->
                createMsg input
                |> environment.AuthenticationSystem.DeleteUser
        }