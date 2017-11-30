open System.Numerics
open System

let x = BigInteger 2

let z = BigInteger.Pow (x, 1000)

let strz = string z

strz |> Seq.toArray |> Seq.toList  |> List.map(string) |> List.map(int)  |> List.sum