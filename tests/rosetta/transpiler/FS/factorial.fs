// Generated 2025-08-04 20:44 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec factorial (n: int) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable n = n
    try
        let mutable r: bigint = bigint 1
        let mutable i: int = 2
        while i <= n do
            r <- r * (bigint i)
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
for i in 0 .. (11 - 1) do
    printfn "%s" (((string (i)) + " ") + (string (factorial (i))))
printfn "%s" ("100 " + (string (factorial (100))))
printfn "%s" ("800 " + (string (factorial (800))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
