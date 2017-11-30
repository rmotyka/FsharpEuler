// Learn more about F# at http://fsharp.org

open System
open FSharp.Collections.Array.Parallel

[<EntryPoint>]
let main argv =
    let isEven n =
        n % 2 = 0

    let collatz input  =
        let rec aux n counter = 
            if n = 1 then
                counter
            else
                if isEven n then
                    aux (n / 2) (counter + 1)
                else
                    aux (3 * n + 1) (counter + 1)
        aux input 1

    let result = 
        [|1 .. (1_000_000 - 1)|]
        |> FSharp.Collections.Array.Parallel.map(fun x -> (x, collatz x))
        |> Array.maxBy(snd)
    printfn "result %A" result
    0 // return an integer exit code
