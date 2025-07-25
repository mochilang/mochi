// Generated 2025-07-25 17:29 +0000

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
let rec merge (``base``: Map<string, obj>) (update: Map<string, obj>) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable ``base`` = ``base``
    let mutable update = update
    try
        let mutable result: Map<string, obj> = Map.ofList []
        for KeyValue(k, _) in ``base`` do
            result <- Map.add k (``base``.[k]) result
        for KeyValue(k, _) in update do
            result <- Map.add k (update.[k]) result
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let ``base``: Map<string, obj> = Map.ofList [("name", box "Rocket Skates"); ("price", box 12.75); ("color", box "yellow")]
        let update: Map<string, obj> = Map.ofList [("price", box 15.25); ("color", box "red"); ("year", box 1974)]
        let result: Map<string, obj> = merge ``base`` update
        printfn "%A" result
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
