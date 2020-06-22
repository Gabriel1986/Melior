module Server.Organizations.ViesService

open Padi.Vies
open System.Net.Http
open Server.Library
open Shared.Library
open Shared.Read
open Shared.ConstrainedTypes
open Serilog

let private httpClient = new HttpClient()

let private mapViesResponse (viesResponse: ViesCheckVatResponse): VatNumberValidationResponse = {
    CountryCode = viesResponse.CountryCode
    VatNumber = viesResponse.VatNumber
    RequestDate = viesResponse.RequestDate
    IsValid = viesResponse.IsValid
    Name = viesResponse.Name |> String.toOption
    Address = viesResponse.Address |> String.toOption
}

let verifyVatNumber (vatNumber: VatNumber) =
    use manager = new ViesManager(httpClient)
    try
        manager.IsActive(vatNumber.CountryCode, vatNumber.Value)
        |> Async.AwaitTask
        |> Async.map (mapViesResponse >> Ok)
    with ex ->
        match ex with
        | :? ViesServiceException -> 
            Log.Logger.Error(ex, "Er is iets misgelopen bij het opvragen van de BTW gegevens bij VIES")
            Async.lift (Error ("Er is iets misgelopen bij het opvragen van de BTW gegevens bij de webservice van de EU. Gelieve het later nog eens te proberen."))
        | someOtherException ->
            raise someOtherException