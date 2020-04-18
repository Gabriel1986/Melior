namespace Client

open Elmish
open Elmish.Debug
open Elmish.React
open Elmish.Navigation
open Fable.React
open Shared

module Client =
    type ClientState = {
        Forecasts: WeatherForecast[]
    }

    type ClientMessage =
        | ReceivedForecasts of WeatherForecast []

    let init () =
        let cmd =
            Cmd.OfPromise.perform
                (fun () -> Thoth.Fetch.Fetch.get ("/api/WeatherForecast", caseStrategy = Thoth.Json.CaseStrategy.CamelCase))
                ()
                (fun foreCasts -> ReceivedForecasts foreCasts)

        { Forecasts = [||] }, cmd

    let update (message: ClientMessage) (state: ClientState) =
        match message with
        | ReceivedForecasts foreCasts -> 
            { state with Forecasts = foreCasts }, Cmd.none

    let view (state: ClientState) (dispatch: ClientMessage -> unit) =
        match state.Forecasts with
        | [||] ->
            div [] [
                h2 [] [ str "Hello world." ]
            ]
        | foreCasts ->
            div [] [
                for forecast in foreCasts do
                    yield div [] [ str (forecast.Date.ToString("O")) ]
                    yield div [] [ str (string forecast.TemperatureC) ]
                    yield div [] [ str (string forecast.TemperatureF) ]
                    yield div [] [ str forecast.Summary ]
                    yield hr []
            ]

    Program.mkProgram init update view
    #if DEBUG
    |> Program.withConsoleTrace
    #endif
    |> Program.withReactBatched "elmish-app"
    #if DEBUG
    |> Program.withDebugger
    #endif
    //|> Program.withSubscription poller
    |> Program.run
