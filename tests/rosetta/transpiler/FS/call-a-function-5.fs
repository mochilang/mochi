// Generated 2025-07-27 10:08 +0000

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
let rec doIt (p: Map<string, int>) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable p = p
    try
        let mutable b: int = 0
        if Map.containsKey "b" p then
            b <- unbox<int> (p.["b"] |> unbox<int>)
        __ret <- unbox<int> (((unbox<int> (p.["a"] |> unbox<int>)) + b) + (p.["c"] |> unbox<int>))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable p: Map<string, int> = Map.ofList []
        p <- Map.add "a" 1 p
        p <- Map.add "c" 9 p
        printfn "%s" (string (doIt p))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
