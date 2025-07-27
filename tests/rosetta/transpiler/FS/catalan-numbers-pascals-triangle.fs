// Generated 2025-07-27 23:45 +0700

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
let n: int = 15
let mutable t: int array = [||]
for _ in 0 .. ((n + 2) - 1) do
    t <- unbox<int array> (Array.append t [|0|])
t.[1] <- 1
for i in 1 .. ((n + 1) - 1) do
    let mutable j: int = i
    while j > 1 do
        t.[j] <- (t.[j]) + (t.[j - 1])
        j <- j - 1
    t.[unbox<int> (i + 1)] <- t.[i]
    j <- i + 1
    while j > 1 do
        t.[j] <- (t.[j]) + (t.[j - 1])
        j <- j - 1
    let cat: int = (t.[i + 1]) - (t.[i])
    if i < 10 then
        printfn "%s" (((" " + (string i)) + " : ") + (string cat))
    else
        printfn "%s" (((string i) + " : ") + (string cat))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
