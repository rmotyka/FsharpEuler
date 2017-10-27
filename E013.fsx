let isEven n =
    n % 2 = 0

let collatz input  =
    let rec aux n counter = 
        printfn "%A" n
        if n = 1 then
            counter
        else
            if isEven n then
                aux (n / 2) (counter + 1)
            else
                aux (3 * n + 1) (counter + 1)
    aux input 1

collatz 13