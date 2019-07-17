module MappingTools

open System

let delimiter = ' '

let stringOfIntsToIntList (string: string) =
    string.Split [| delimiter |] |> Seq.filter ((<>) "") |> Seq.map int |> Seq.toList

let intsListToStringOfInts (ints: int list) =
    ints |> List.map string |> fun x -> String.Join(delimiter, x)

let stringOfMinutesToTimeSpanList (string: string) =
    string.Split [| delimiter |] |> Seq.map (Double.Parse >> TimeSpan.FromMinutes) |> Seq.toList

let timeSpanListToStringOfMinutes (timeSpans: TimeSpan list) =
    timeSpans |> List.map (fun x -> x.TotalMinutes.ToString()) |> fun x -> String.Join(delimiter, x)

let stringIntToBool =
    function
    | "0" -> false
    | _ -> true

let boolToString =
    function
    | false -> "0"
    | true -> "1"

// https://en.wikipedia.org/wiki/C0_and_C1_control_codes

let split separator (string: string) =
    string.Split [| separator |] |> Array.toList

let splitByFileSeparator =
    split '\x1c'

let splitByGroupSeparator =
    split '\x1d'

let splitByRecordSeparator =
    split '\x1e'

let splitByUnitSeparator =
    split '\x1f'

let join separator (strings: string list) =
    String.Join(string separator, strings)

let joinByFileSeparator =
    join '\x1c'

let joinByGroupSeparator =
    join '\x1d'

let joinByRecordSeparator =
    join '\x1e'

let joinByUnitSeparator =
    join '\x1f'

let cutOffInt16 x =
    if x > float Int16.MaxValue
    then Int16.MaxValue
    else int16 x

let round (dt: DateTime) (d: TimeSpan) = // https://stackoverflow.com/a/20046261/
    let delta = dt.Ticks % d.Ticks
    let roundUp = delta > d.Ticks / 2L
    let offset = if roundUp then d.Ticks else 0L
    DateTime(dt.Ticks + offset - delta, dt.Kind)
