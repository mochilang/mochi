// Generated 2025-08-02 00:26 +0700

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
let max: int = 20
let words = Map.ofList [(3, "Fizz"); (5, "Buzz"); (7, "Baxx")]
let keys: int array = [|3; 5; 7|]
for i in 1 .. ((max + 1) - 1) do
    let mutable text: string = ""
    for n in keys do
        if (((i % n + n) % n)) = 0 then
            text <- text + (unbox<string> (words.[n]))
    if text = "" then
        text <- string i
    printfn "%s" text
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
