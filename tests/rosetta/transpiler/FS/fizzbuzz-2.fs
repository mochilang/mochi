// Generated 2025-07-30 21:05 +0700

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
for i in 1 .. (101 - 1) do
    let m: Map<string, Map<string, string>> = Map.ofList [(false, Map.ofList [(false, string i); (true, "Fizz")]); (true, Map.ofList [(false, "Buzz"); (true, "FizzBuzz")])]
    printfn "%A" ((m.[((((i % 5 + 5) % 5)) = 0)] |> unbox<string>>).[(((i % 3 + 3) % 3)) = 0])
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
