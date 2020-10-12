module Server.Organizations.ViesService

open Padi.Vies
open System.Net.Http
open Server.Library
open Shared.Library
open Shared.Read
open Shared.ConstrainedTypes
open Serilog
open Server.AppSettings

let private httpClient = new HttpClient()

let private mapViesResponse (appSettings: AppSettings) (viesResponse: ViesCheckVatResponse): Async<VatNumberValidationResponse> = async {    
    let! address =
        match viesResponse.Address |> String.toOption with
        | Some address -> GoogleGeocodingApi.parseAddress httpClient appSettings address
        | None -> Async.lift None

    return {
        CountryCode = viesResponse.CountryCode
        VatNumber = viesResponse.VatNumber
        RequestDate = viesResponse.RequestDate
        IsValid = viesResponse.IsValid
        Name = viesResponse.Name |> String.toOption
        Address = address
    }
}

let verifyVatNumber (settings: AppSettings) (vatNumber: VatNumber) =
    use manager = new ViesManager(httpClient)
    async {
        try
            let! result =
                manager.IsActive(vatNumber.CountryCode (), vatNumber.Value ())
                |> Async.AwaitTask
                |> Async.bind (mapViesResponse settings)
            return Ok result
        with ex ->
            match ex with
            | :? System.AggregateException as ex ->
                match ex.InnerException with
                | :? ViesServiceException ->
                    Log.Logger.Error(ex, "Er is iets misgelopen bij het opvragen van de BTW gegevens bij VIES")
                    return Error "Er is iets misgelopen bij het opvragen van de BTW gegevens bij de webservice van de EU. Gelieve het later nog eens te proberen."
                | :? ViesValidationException ->
                    return Error "U heeft een ongeldig BTW nummer ingevoerd"
                | someOtherException ->
                    return raise someOtherException
            | someOtherException ->
                return raise someOtherException
    }