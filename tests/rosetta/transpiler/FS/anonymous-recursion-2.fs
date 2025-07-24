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
        for i in [|-1; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10|] do
            if i < 0 then
                printfn "%s" (("fib(" + (string i)) + ") returned error: negative n is forbidden")
            else
                printfn "%s" ((("fib(" + (string i)) + ") = ") + (string (fib i)))
        __ret
    with
        | Return -> __ret
main()
