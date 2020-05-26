module Server.PostgreSQL.Sql

open Npgsql.FSharp

let readSingle<'T> (reader: RowReader -> 'T) props =
    Sql.executeRowAsync reader props
    |> Async.map (
        function
        | Ok r -> Some r
        | Error e -> 
            Serilog.Log.Logger.Error (e, "Something went wrong while trying to retrieve a single row of data.")
            None
    )

let read<'T> (reader: RowReader -> 'T) props =
    Sql.executeAsync reader props
    |> Async.map (
        function
        | Ok r -> r
        | Error e -> 
            Serilog.Log.Logger.Error (e, "Something went wrong while trying to retrieve multiple rows of data.")
            []
    )

let writeAsync props =
    Sql.executeNonQueryAsync props
    |> Async.map (
        function
        | Ok numberOfRows -> 
            ()
        | Error e ->
            raise e
    )

type CaseInsensitiveRowReader (rowReader: RowReader) =
    member _.RowReader = rowReader
    member me.bool (s: string)           = me.RowReader.bool (s.ToLower())
    member me.boolOrNone (s: string)     = me.RowReader.boolOrNone (s.ToLower())
    member me.string (s: string)         = me.RowReader.string (s.ToLower())
    member me.stringOrNone (s: string)   = me.RowReader.stringOrNone (s.ToLower())
    member me.uuid (s: string)           = me.RowReader.uuid (s.ToLower())
    member me.uuidOrNone (s: string)     = me.RowReader.uuidOrNone (s.ToLower())
    member me.int (s: string)            = me.RowReader.int (s.ToLower())
    member me.intOrNone (s: string)      = me.RowReader.intOrNone (s.ToLower())
    member me.dateTime (s: string)       = me.RowReader.dateTime (s.ToLower())
    member me.dateTimeOrNone (s: string) = me.RowReader.dateTimeOrNone (s.ToLower())