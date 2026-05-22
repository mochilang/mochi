// Generated 2025-07-25 07:54 +0700

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
let n: int = 1000
let mutable s: int = 0
for i in 1 .. (n - 1) do
    s <- s + i
let __bench_end = _now()
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": 0,\n  \"name\": \"simple\"\n}" ((__bench_end - __bench_start) / 1000)
