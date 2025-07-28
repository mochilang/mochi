// Generated 2025-07-28 07:48 +0700

exception Break
exception Continue

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

let nPts: int = 100
let rMin: int = 10
let rMax: int = 15
let span: int = (rMax + 1) + rMax
let mutable rows: string array array = [||]
let mutable r: int = 0
while r < span do
    let mutable row: string array = [||]
    let mutable c: int = 0
    while c < (span * 2) do
        row <- Array.append row [|" "|]
        c <- c + 1
    rows <- Array.append rows [|row|]
    r <- r + 1
let mutable u: int = 0
let mutable seen: Map<string, bool> = Map.ofList []
let min2: int = rMin * rMin
let max2: int = rMax * rMax
let mutable n: int = 0
try
    while n < nPts do
        try
            let mutable x: bigint = (int ((((_now()) % span + span) % span))) - rMax
            let mutable y: bigint = (int ((((_now()) % span + span) % span))) - rMax
            let rs: bigint = (x * x) + (y * y)
            if (rs < (bigint min2)) || (rs > (bigint max2)) then
                raise Continue
            n <- n + 1
            let row: bigint = y + (bigint rMax)
            let col: bigint = (x + (bigint rMax)) * (bigint 2)
            (rows.[row]).[col] <- "*"
            let key: string = ((string row) + ",") + (string col)
            if not (seen.[key] |> unbox<bool>) then
                seen <- Map.add key true seen
                u <- u + 1
        with
        | Continue -> ()
        | Break -> raise Break
with
| Break -> ()
| Continue -> ()
let mutable i: int = 0
while i < span do
    let mutable line: string = ""
    let mutable j: int = 0
    while j < (span * 2) do
        line <- line + ((rows.[i]).[j])
        j <- j + 1
    printfn "%s" line
    i <- i + 1
printfn "%s" ((string u) + " unique points")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
