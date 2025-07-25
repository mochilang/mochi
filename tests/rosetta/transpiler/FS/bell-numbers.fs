// Generated 2025-07-26 04:38 +0700

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
let rec bellTriangle (n: int) =
    let mutable __ret : bigint array array = Unchecked.defaultof<bigint array array>
    let mutable n = n
    try
        let mutable tri: bigint array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable row: bigint array = [||]
            let mutable j: int = 0
            while j < i do
                row <- Array.append row [|bigint 0|]
                j <- j + 1
            tri <- Array.append tri [|row|]
            i <- i + 1
        (tri.[1]).[0] <- 1
        i <- 2
        while i < n do
            (tri.[i]).[0] <- (tri.[i - 1]).[i - 2]
            let mutable j: int = 1
            while j < i do
                (tri.[i]).[j] <- ((tri.[i]).[j - 1]) + ((tri.[i - 1]).[j - 1])
                j <- j + 1
            i <- i + 1
        __ret <- tri
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let bt: bigint array array = bellTriangle 51
        printfn "%s" "First fifteen and fiftieth Bell numbers:"
        for i in 1 .. (16 - 1) do
            printfn "%s" ((("" + (unbox<string> (padStart (string i) 2 " "))) + ": ") + (string ((bt.[i]).[0])))
        printfn "%s" ("50: " + (string ((bt.[50]).[0])))
        printfn "%s" ""
        printfn "%s" "The first ten rows of Bell's triangle:"
        for i in 1 .. (11 - 1) do
            printfn "%A" (bt.[i])
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
