// Generated 2025-07-25 12:29 +0700

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
let mutable doors = [||]
for i in 0 .. (100 - 1) do
    doors <- Array.append doors [|false|]
for pass in 1 .. (101 - 1) do
    let mutable idx = pass - 1
    while idx < 100 do
        doors.[idx] <- not (doors.[idx])
        idx <- idx + pass
for row in 0 .. (10 - 1) do
    let mutable line: string = ""
    for col in 0 .. (10 - 1) do
        let idx = (row * 10) + col
        if doors.[idx] then
            line <- line + "1"
        else
            line <- line + "0"
        if col < 9 then
            line <- line + " "
    printfn "%s" line
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
