// Generated 2025-07-31 00:10 +0700

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
printfn "%s" "Police  Sanitation  Fire"
printfn "%s" "------  ----------  ----"
let mutable count: int = 0
let mutable i: int = 2
while i < 7 do
    let mutable j: int = 1
    while j < 8 do
        if j <> i then
            let mutable k: int = 1
            while k < 8 do
                if (k <> i) && (k <> j) then
                    if ((i + j) + k) = 12 then
                        printfn "%s" ((((("  " + (string i)) + "         ") + (string j)) + "         ") + (string k))
                        count <- count + 1
                k <- k + 1
        j <- j + 1
    i <- i + 2
printfn "%s" ""
printfn "%s" ((string count) + " valid combinations")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
