// Generated 2025-07-25 14:38 +0000

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
let msg: string = "Hello World! "
let mutable shift: int = 0
let mutable inc: int = 1
let mutable clicks: int = 0
let mutable frames: int = 0
while clicks < 5 do
    let mutable line: string = ""
    let mutable i: int = 0
    while i < (String.length msg) do
        let idx: int = (shift + i) % (String.length msg)
        line <- line + (msg.Substring(idx, (idx + 1) - idx))
        i <- i + 1
    printfn "%s" line
    shift <- (shift + inc) % (String.length msg)
    frames <- frames + 1
    if (frames % (String.length msg)) = 0 then
        inc <- (String.length msg) - inc
        clicks <- clicks + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
