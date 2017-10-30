open System
open System.IO
open Microsoft.FSharp.Math

let addBigint x y =
    bigint.Add(x, y)

let calcSum () =
    let restult = 
        File.ReadAllLines("resources/E13data.txt")
        |> Array.filter(fun x -> not (String.IsNullOrWhiteSpace(x)))
        |> Array.map(fun x -> x.Trim())
        |> Array.map(fun x -> bigint.Parse(x))
        |> Array.sum
        |> sprintf "%A"
    restult.[0 .. 9]


calcSum()