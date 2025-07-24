// Generated 2025-07-25 00:53 +0700

exception Return

let rec pow_int (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable b: int = ``base``
        let mutable e: int = exp
        while e > 0 do
            if (e % 2) = 1 then
                result <- result * b
            b <- b * b
            e <- int (e / 2)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and pow_big (``base``: bigint) (exp: int) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: bigint = bigint 1
        let mutable b: bigint = ``base``
        let mutable e: int = exp
        while e > 0 do
            if (e % 2) = 1 then
                result <- result * b
            b <- b * b
            e <- int (e / 2)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let mutable e1 = pow_int 3 2
let mutable e2 = pow_int 4 e1
let mutable ``base``: bigint = bigint 5
let mutable x: bigint = pow_big ``base`` e2
let mutable s: string = string x
printfn "%s" (String.concat " " [|sprintf "%A" "5^(4^(3^2)) has"; sprintf "%d" (String.length s); sprintf "%A" "digits:"; sprintf "%A" (s.Substring(0, 20 - 0)); sprintf "%A" "..."; sprintf "%A" (s.Substring((String.length s) - 20, (String.length s) - ((String.length s) - 20)))|])
