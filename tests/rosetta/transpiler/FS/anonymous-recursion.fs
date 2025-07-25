// Generated 2025-07-26 05:05 +0700

exception Return

let rec fib (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- if n < 2 then n else ((fib (n - 1)) + (fib (n - 2)))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable i: int = -1
        while i <= 10 do
            if i < 0 then
                printfn "%s" (("fib(" + (string i)) + ") returned error: negative n is forbidden")
            else
                printfn "%s" ((("fib(" + (string i)) + ") = ") + (string (fib i)))
            i <- i + 1
        __ret
    with
        | Return -> __ret
main()
