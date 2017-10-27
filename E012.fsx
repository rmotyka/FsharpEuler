let generateTriangeNumbers = 
    Seq.unfold(fun (index, tri) -> Some ((index, tri), (index + 1L, tri + index + 1L))) (1L, 1L)

// let findFactorsOf n  =
//     [1L..n]
//     |> Seq.filter (fun x -> n % x = 0L)

let findFactorsOf(n:int64) =
    let upperBound = int64(sqrt(double(n)))
    [1L..upperBound] 
    |> Seq.filter (fun x -> n % x = 0L) 
    |> Seq.collect (fun x -> [x; n/x])

let numberOfFactors n =
    n |> findFactorsOf |> Seq.length

generateTriangeNumbers 
|> Seq.map(fun (index, tri) -> (tri, numberOfFactors tri))
|> Seq.find(fun (tri, factors) -> factors >= 500)

//generateTriangeNumbers |> Seq.take 10 |> Seq.toList

(*

1
1+2
1+2+3
1+2+3+4

*)

