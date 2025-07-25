// Generated 2025-07-26 05:05 +0700

exception Return

let rec kPrime (n: int) (k: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    let mutable k = k
    try
        let mutable nf: int = 0
        let mutable i: int = 2
        while i <= n do
            while (((n % i + i) % i)) = 0 do
                if nf = k then
                    __ret <- false
                    raise Return
                nf <- nf + 1
                n <- n / i
            i <- i + 1
        __ret <- nf = k
        raise Return
        __ret
    with
        | Return -> __ret
and gen (k: int) (count: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable k = k
    let mutable count = count
    try
        let mutable r: int array = [||]
        let mutable n: int = 2
        while (int (Array.length r)) < count do
            if unbox<bool> (kPrime n k) then
                r <- Array.append r [|n|]
            n <- n + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable k: int = 1
        while k <= 5 do
            printfn "%s" (((string k) + " ") + (string (gen k 10)))
            k <- k + 1
        __ret
    with
        | Return -> __ret
main()
