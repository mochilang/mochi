// Generated 2025-07-26 00:23 +0700

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
let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let m: Map<string, int> = Map.ofList [("hello", 13); ("world", 31); ("!", 71)]
        for k in List.map fst (Map.toList m) do
            printfn "%s" ((("key = " + (unbox<string> k)) + ", value = ") + (string (m.[k] |> unbox<int>)))
        for k in List.map fst (Map.toList m) do
            printfn "%s" ("key = " + (unbox<string> k))
        for k in List.map fst (Map.toList m) do
            printfn "%s" ("value = " + (string (m.[k] |> unbox<int>)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
