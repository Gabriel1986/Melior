namespace Server

open System
open Microsoft.AspNetCore.Http

module Library =
    type Message<'T> = {
        CreatedAt: DateTimeOffset
        Context: HttpContext
        Payload: 'T
    }