// Generated 2025-07-25 00:53 +0700

exception Return

let rec fib (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 2 then
            __ret <- n
            raise Return
        let mutable a: int = 0
        let mutable b: int = 1
        let mutable i: int = 1
        while i < n do
            let t: int = a + b
            a <- b
            b <- t
            i <- i + 1
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        for n in [|0; 1; 2; 3; 4; 5; 10; 40; -1|] do
            if n < 0 then
                printfn "%s" "fib undefined for negative numbers"
            else
                printfn "%s" ((("fib " + (string n)) + " = ") + (string (fib n)))
        __ret
    with
        | Return -> __ret
main()
