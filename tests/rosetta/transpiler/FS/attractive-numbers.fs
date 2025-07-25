// Generated 2025-07-25 17:33 +0000

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
        if (((n % 3 + 3) % 3)) = 0 then
            __ret <- n = 3
            raise Return
        let mutable d: int = 5
        while (d * d) <= n do
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 2
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 4
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and countPrimeFactors (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n = 1 then
            __ret <- 0
            raise Return
        if isPrime n then
            __ret <- 1
            raise Return
        let mutable count: int = 0
        let mutable f: int = 2
        while true do
            if (((n % f + f) % f)) = 0 then
                count <- count + 1
                n <- n / f
                if n = 1 then
                    __ret <- count
                    raise Return
                if isPrime n then
                    f <- n
            else
                if f >= 3 then
                    f <- f + 2
                else
                    f <- 3
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and pad4 (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string n
        while (String.length s) < 4 do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let max: int = 120
        printfn "%s" (("The attractive numbers up to and including " + (string max)) + " are:")
        let mutable count: int = 0
        let mutable line: string = ""
        let mutable lineCount: int = 0
        let mutable i: int = 1
        while i <= max do
            let c: int = countPrimeFactors i
            if isPrime c then
                line <- line + (unbox<string> (pad4 i))
                count <- count + 1
                lineCount <- lineCount + 1
                if lineCount = 20 then
                    printfn "%s" line
                    line <- ""
                    lineCount <- 0
            i <- i + 1
        if lineCount > 0 then
            printfn "%s" line
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
