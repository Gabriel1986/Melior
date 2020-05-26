module Server.Buildings.Workflow

open System
open NodaTime
open Shared
open Shared.Remoting
open Shared.Buildings
open Shared.Trial.Control
open Shared.Library
open Shared.ConstrainedTypes
open Shared.Domain
open Server.Library
open Server.Buildings.Storage
open Server.Addresses.Workflow

type SharedConciergeId = Shared.Buildings.ConciergeId
type SharedSyndicId = Shared.Buildings.SyndicId

let private validateLocalDate year month day =
    try
        let from = LocalDate(year, month, day)
        Trial.Pass from
    with e ->
        Trial.ofError "De datum is ongeldig"

let private validateGeneralMeetingPeriod (period: GeneralMeetingPeriod) =
    let today = DateTime.Today
    trial {
        from from in validateLocalDate today.Year period.FromMonth period.FromDay
        also until in validateLocalDate today.Year period.UntilMonth period.UntilDay

        yield (from, until)
    }

let private mapConciergeId (concierge: SharedConciergeId): ConciergeId = 
    match concierge with
    | SharedConciergeId.OwnerId ownerId     -> ConciergeId.OwnerId ownerId
    | SharedConciergeId.NonOwnerId personId -> ConciergeId.PersonId personId

let private mapSyndicId (syndic: SharedSyndicId): SyndicId =
    match syndic with
    | SharedSyndicId.OwnerId ownerId             -> SyndicId.OwnerId ownerId
    | SharedSyndicId.ProfessionalSyndicId profId -> SyndicId.ProfessionalSyndicId profId
    | SharedSyndicId.OtherId personId            -> SyndicId.PersonId personId


let private validateBuilding (req: BuildingRequest) =
    trial {
        from code in String16.Of req.Code
        also name in String255.Of req.Name
        also yearOfConstruction in validateOptional PositiveInt.Of req.YearOfConstruction
        also yearOfDelivery in validateOptional PositiveInt.Of req.YearOfDelivery
        also validatedPeriod in validateOptional validateGeneralMeetingPeriod req.GeneralMeetingPeriod
        also address in validateAddress req.Address

        yield {
            BuildingId = req.BuildingId
            Code = code
            Name = name
            Address = address
            OrganizationNumber = req.OrganizationNumber
            Remarks = req.Remarks
            GeneralMeetingFrom = validatedPeriod |> Option.map (fun p -> fst p)
            GeneralMeetingUntil = validatedPeriod |> Option.map (fun p -> snd p)
            ConciergeId = req.ConciergeId |> Option.map mapConciergeId
            SyndicId = req.SyndicId |> Option.map mapSyndicId 
            YearOfConstruction = yearOfConstruction
            YearOfDelivery = yearOfDelivery
        }
    }

let createBuilding (connectionString: string) (msg: Message<BuildingRequest>): Async<Result<unit, CreateBuildingError>> = async {
    let result = validateBuilding msg.Payload
    match result with
    | Trial.Pass building ->
        do! Server.Buildings.Storage.createBuilding connectionString building
        return Ok ()
    | Trial.Fail errors ->
        return Error
            (CreateBuildingError.ValidationError {
                GeneralError = errors
                NameError = []
                CodeError = []
                AddressError = []
                GeneralMeetingDateError = []
            })
}

let updateBuilding (connectionString) (msg: Message<BuildingRequest>): Async<Result<unit, UpdateBuildingError>> = async {
    let result = validateBuilding msg.Payload
    match result with
    | Trial.Pass building ->
        do! Server.Buildings.Storage.updateBuilding connectionString building
        return Ok ()
    | Trial.Fail errors ->
        return Error
            (UpdateBuildingError.ValidationError {
                GeneralError = errors
                NameError = []
                CodeError = []
                AddressError = []
                GeneralMeetingDateError = []
            })
}

let deleteBuilding (connectionString: string) (msg: Message<Guid>): Async<Result<unit, AuthorizationError>> =
    async { return Ok () }
