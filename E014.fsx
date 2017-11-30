let isEven n =
    n % 2 = 0

let collatz input  =
    printfn "%d" input
    let rec aux n counter = 
        if n = 1 then
            counter
        else
            if isEven n then
                aux (n / 2) (counter + 1)
            else
                aux (3 * n + 1) (counter + 1)
    aux input 1

[500000 .. (1000000 - 1)]
|> List.maxBy(collatz)let isEven n =
    n % 2 = 0

let collatz input  =
    printfn "%d" input
    let rec aux n counter = 
        if n = 1 then
            counter
        else
            if isEven n then
                aux (n / 2) (counter + 1)
            else
                aux (3 * n + 1) (counter + 1)
    aux input 1

[500000 .. (1000000 - 1)]
|> List.maxBy(collatz)


let nextNumber n = if n%2L = 0L then n/2L else 3L*n+1L
 
let findSequenceLength n =
    let mutable count = 1L
    let mutable current = n
 
    while current > 1L do
        current <- nextNumber current
        count <- count + 1L
    count
 
let longestSeq = [|1L..999999L|] |> Seq.maxBy findSequenceLength