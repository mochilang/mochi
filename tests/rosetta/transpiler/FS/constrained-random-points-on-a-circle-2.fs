// Generated 2025-07-28 07:48 +0700

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
let mutable poss: int array array = [||]
let min2: int = rMin * rMin
let max2: int = rMax * rMax
let mutable y: int = -rMax
while y <= rMax do
    let mutable x: int = -rMax
    while x <= rMax do
        let r2: int = (x * x) + (y * y)
        if (r2 >= min2) && (r2 <= max2) then
            poss <- Array.append poss [|[|x; y|]|]
        x <- x + 1
    y <- y + 1
printfn "%s" ((string (Seq.length poss)) + " possible points")
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
let mutable n: int = 0
while n < nPts do
    let mutable i: int = (((_now()) % (Seq.length poss) + (Seq.length poss)) % (Seq.length poss))
    let x: int = (poss.[i]).[0]
    let yy: int = (poss.[i]).[1]
    let row: int = yy + rMax
    let col: int = (x + rMax) * 2
    (rows.[row]).[col] <- "*"
    let key: string = ((string row) + ",") + (string col)
    if not (seen.[key] |> unbox<bool>) then
        seen <- Map.add key true seen
        u <- u + 1
    n <- n + 1
let mutable i2: int = 0
while i2 < span do
    let mutable line: string = ""
    let mutable j: int = 0
    while j < (span * 2) do
        line <- line + ((rows.[i2]).[j])
        j <- j + 1
    printfn "%s" line
    i2 <- i2 + 1
printfn "%s" ((string u) + " unique points")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
