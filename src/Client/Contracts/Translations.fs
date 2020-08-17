module Client.Contracts.Translations

open Shared.Read

let translateQuestion (question: ContractTypeQuestion) =
    match question with
    | BuildingHasElevator -> "Lift"
    | BuildingHasCommonCentralHeating -> "Gemeenschappelijke CV"
    | BuildingHasFireAlarm -> "Brandalarm"
    | BuildingHasFireExtinguisher -> "Brandblusapparaat"
    | BuildingHasFireHoseReel -> "Brandhaspel"

let translatePredefinedType (predefined: PredefinedContractType) =
    match predefined with
    | ElevatorMaintenance -> "Onderhoud lift"
    | ElevatorInspection -> "Keuring lift"
    | CommonCentralHeatingInspection -> "Onderhoud gem. CV"
    | FireAlarmInspection -> "Keuring brandalarm"
    | FireExtinguisherInspection -> "Keuring brandblusapparaten"
    | FireHoseReelInspection -> "Keuring brandhaspels"
    | FireInsurance -> "Brandverzekering"
    | LiabilityInsurance -> "Aansprakelijkheidsverzekering"
    | CivilLiabilityForCoOwnerCouncil -> "B.A. VME"
    | ElectricitySupplier -> "Leverancier electriciteit"
    | WaterSupplier -> "Leverancier water"