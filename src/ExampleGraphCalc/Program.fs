// Learn more about F# at http://fsharp.org

open System
open Fetisch
open FSharp.Charting

[<EntryPoint>]
let main argv =
    Chart.Line([for x in 0.0 .. 0.2 .. 10.0 -> (x, 4.0 * x)], Name="f")
    |> Chart.Show
    //Chart.Combine ([
    //    Chart.Line([for x in 0.0 .. 0.2 .. 10.0 -> (x, 4.0 * x)], Name="f")
    //    Chart.Line([for x in 0.0 .. 0.2 .. 10.0 -> (x, 3.0 * x + 5.0)], Name="g")
    //])
    //|> Chart.Show

    0 // return an integer exit code
