// Generated 2025-07-25 14:38 +0000

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
let rec countDivisors (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 2 then
            __ret <- 1
            raise Return
        let mutable count: int = 2
        let mutable i: int = 2
        while i <= (n / 2) do
            if (n % i) = 0 then
                count <- count + 1
            i <- i + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" "The first 20 anti-primes are:"
        let mutable maxDiv: int = 0
        let mutable count: int = 0
        let mutable n: int = 1
        let mutable line: string = ""
        while count < 20 do
            let d: int = countDivisors n
            if d > maxDiv then
                line <- (line + (string n)) + " "
                maxDiv <- d
                count <- count + 1
            n <- n + 1
        line <- line.Substring(0, ((String.length line) - 1) - 0)
        printfn "%s" line
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
