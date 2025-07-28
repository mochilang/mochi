// Generated 2025-07-28 10:03 +0700

exception Break
exception Continue

exception Return

let mutable _nowSeed:int64 = 0L
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- int64 v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525L + 1013904223L) % 2147483647L
        int _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)

_initNow()
let rec isPrime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        if (((n % 2 + 2) % 2)) = 0 then
            __ret <- n = 2
            raise Return
        let mutable d: int = 3
        while (d * d) <= n do
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 2
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and revInt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable r: int = 0
        let mutable t: int = n
        while t > 0 do
            r <- (r * 10) + (((t % 10 + 10) % 10))
            t <- int (t / 10)
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable emirps: int array = [||]
        let mutable n: int = 2
        while (Seq.length emirps) < 10000 do
            if isPrime n then
                let r: int = revInt n
                if (r <> n) && (unbox<bool> (isPrime r)) then
                    emirps <- Array.append emirps [|n|]
            n <- n + 1
        let mutable line: string = "   ["
        let mutable i: int = 0
        while i < 20 do
            line <- line + (string (emirps.[i]))
            if i < 19 then
                line <- line + ", "
            i <- i + 1
        line <- line + "]"
        printfn "%s" "First 20:"
        printfn "%s" line
        line <- "  ["
        try
            for e in emirps do
                try
                    if e >= 8000 then
                        raise Break
                    if e >= 7700 then
                        line <- (line + (string e)) + ", "
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        line <- line + "]"
        printfn "%s" "Between 7700 and 8000:"
        printfn "%s" line
        printfn "%s" "10000th:"
        printfn "%s" (("   [" + (string (emirps.[9999]))) + "]")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
