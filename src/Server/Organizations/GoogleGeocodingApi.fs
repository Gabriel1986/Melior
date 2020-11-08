module Server.Organizations.GoogleGeocodingApi

open System.Net.Http
open Shared.Read
open Server.AppSettings
open FSharp.Data
open Shared.Library

let [<Literal>] example =
    """
        {
            "results": [
                {
                    "address_components": [
                        {
                            "long_name": "X",
                            "short_name": "Y",
                            "types": [ "route" ]
                        }
                    ],
                    "formatted_address": "Some address",
                    "geometry": {
                        "location": {
                            "lat": 0.0,
                            "lng": 0.0
                        },
                        "location_type": "RANGE_INTERPOLATED",
                        "viewport": {
                            "northeast": {
                                "lat": 0.0,
                                "lng": 0.0
                            },
                            "southwest": {
                                "lat": 0.0,
                                "lng": 0.0
                            }
                        }
                    },
                    "place_id": "Some place id",
                    "types": [ "street_address" ]
                }
            ],
            "status": "OK"
        }
    """

let [<Literal>] GeoCodingApiUrl = "https://maps.googleapis.com/maps/api/geocode/json"
type GeoCodingResult = JsonProvider<example>

let parseAddress (httpClient: HttpClient) (settings: AppSettings) (address: string): Async<Address option> = async {
    let! geoResult = 
        httpClient.GetStringAsync(sprintf "%s?address=%s&key=%s&language=nl" GeoCodingApiUrl address settings.Google.CloudApiKey)
        |> Async.AwaitTask

    let parsed = GeoCodingResult.Parse(geoResult)
    if parsed.Status <> "OK" then
        return None
    else
        let addressComponents = 
            parsed.Results 
            |> Array.tryPick (fun r -> if r.Types |> Array.exists (fun t -> t = "street_address" || t = "route") then Some r.AddressComponents else None)
        match addressComponents with
        | Some components ->
            let tryFindAddressComponent componentName =
                components 
                |> Array.tryPick (fun c -> 
                    if c.Types |> Array.contains componentName 
                    then Some c.LongName
                    else None)

            let streetNumber = tryFindAddressComponent "street_number"
            let street = tryFindAddressComponent "route"
            let mailboxNumber = tryFindAddressComponent "post_box"
            let town = tryFindAddressComponent "locality"
            let zipCode = tryFindAddressComponent "postal_code"
            let country = tryFindAddressComponent "country"
            return Some {
                Country = country
                Street = 
                    match [ street; streetNumber ] |> List.choose id with
                    | [] -> None 
                    | xs -> xs |> String.joinWith " " |> Some
                MailboxNumber = mailboxNumber
                Town = town
                ZipCode = zipCode
            }
        | None -> 
            return None
}