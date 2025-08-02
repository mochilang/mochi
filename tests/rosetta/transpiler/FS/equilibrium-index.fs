// Generated 2025-08-02 17:52 +0700

exception Return
let mutable __ret = ()

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
open System

let mutable seed: int = (((_now()) % 2147483647 + 2147483647) % 2147483647)
let rec randN (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        seed <- ((((seed * 1664525) + 1013904223) % 2147483647 + 2147483647) % 2147483647)
        __ret <- ((seed % n + n) % n)
        raise Return
        __ret
    with
        | Return -> __ret
and eqIndices (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable r: int = 0
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            r <- r + (xs.[i])
            i <- i + 1
        let mutable l: int = 0
        let mutable eq: int array = [||]
        i <- 0
        while i < (Seq.length (xs)) do
            r <- r - (xs.[i])
            if l = r then
                eq <- Array.append eq [|i|]
            l <- l + (xs.[i])
            i <- i + 1
        __ret <- eq
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%A" (eqIndices ([|-7; 1; 5; 2; -4; 3; 0|]))
        let mutable verylong: int array = [||]
        let mutable i: int = 0
        while i < 10000 do
            seed <- ((((seed * 1664525) + 1013904223) % 2147483647 + 2147483647) % 2147483647)
            verylong <- Array.append verylong [|(((seed % 1001 + 1001) % 1001)) - 500|]
            i <- i + 1
        printfn "%A" (eqIndices (verylong))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
