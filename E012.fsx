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

let getDivisors (n:int64) = 
    let rec aux n f powers =
        if n > 1L then
            if n % f = 0L then
                aux (n / f) f (f::powers)
            else
                aux n (f + 1L) powers
        else
            powers
    aux n 2L []    

let numberOfFactors n =
    n |> getDivisors |> Seq.length

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

(*
def get_number_of_divisors(n):
    powers = {}
    f = 2
    while n > 1:
        if n % f == 0:
            powers[f] = powers.get(f, 0) + 1
            n /= f
        else:
            f += 1

    if powers:
        return reduce(lambda a,b: a*b, [p+1 for p in powers.values()])
    return 1
*)


