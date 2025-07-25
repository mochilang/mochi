// Generated 2025-07-26 04:38 +0700

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
let target: int = 269696
let modulus: int = 1000000
let mutable n: int = 1
try
    while true do
        let square: int = n * n
        let ending: int = ((square % modulus + modulus) % modulus)
        if ending = target then
            printfn "%s" ((("The smallest number whose square ends with " + (string target)) + " is ") + (string n))
            raise Break
        n <- n + 1
with
| Break -> ()
| Continue -> ()
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
