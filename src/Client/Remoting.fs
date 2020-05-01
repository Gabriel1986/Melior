module Client.Remoting

open Browser
open Shared.Remoting
open Fable.Remoting.Client

let private remotingApi =
    Remoting.createApi ()
    |> Remoting.withBaseUrl(document.baseURI)
    |> Remoting.withRouteBuilder routeBuilder
    |> Remoting.buildProxy<RemotingApi>

let getRemotingApi () = remotingApi