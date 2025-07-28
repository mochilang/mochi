// Generated 2025-07-28 07:48 +0700

exception Return

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
let rec show (n: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    try
        if n = 1 then
            printfn "%s" "1: 1"
            __ret <- ()
            raise Return
        let mutable out: string = (string n) + ": "
        let mutable x: string = ""
        let mutable m: int = n
        let mutable f: int = 2
        while m <> 1 do
            if (((m % f + f) % f)) = 0 then
                out <- (out + x) + (string f)
                x <- "×"
                m <- int (m / f)
            else
                f <- f + 1
        printfn "%s" out
        __ret
    with
        | Return -> __ret
show 1
for i in 2 .. (10 - 1) do
    show i
printfn "%s" "..."
for i in 2144 .. (2155 - 1) do
    show i
printfn "%s" "..."
for i in 9987 .. (10000 - 1) do
    show i
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
