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
let mutable nMech: int = 5
let mutable detailsPerMech: int = 4
for mech in 1 .. ((nMech + 1) - 1) do
    let id: int = mech
    printfn "%s" (((("worker " + (string id)) + " contracted to assemble ") + (string detailsPerMech)) + " details")
    printfn "%s" (("worker " + (string id)) + " enters shop")
    let mutable d: int = 0
    while d < detailsPerMech do
        printfn "%s" (("worker " + (string id)) + " assembling")
        printfn "%s" (("worker " + (string id)) + " completed detail")
        d <- d + 1
    printfn "%s" (("worker " + (string id)) + " leaves shop")
    printfn "%s" (("mechanism " + (string mech)) + " completed")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
