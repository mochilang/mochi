// Generated 2025-07-27 23:45 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System

let rec shuffle (xs: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    try
        let mutable arr: string array = xs
        let mutable i: int = (unbox<int> (Array.length arr)) - 1
        while i > 0 do
            let j = (((_now()) % (i + 1) + (i + 1)) % (i + 1))
            let tmp: string = arr.[i]
            arr.[i] <- arr.[j]
            arr.[j] <- tmp
            i <- i - 1
        __ret <- unbox<string array> arr
        raise Return
        __ret
    with
        | Return -> __ret
for w in shuffle [|"Enjoy"; "Rosetta"; "Code"|] do
    printfn "%s" w
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
