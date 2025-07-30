// Generated 2025-07-30 21:05 +0700

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
let rec findDuplicates (fs: Map<string, string>) (paths: string array) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable fs = fs
    let mutable paths = paths
    try
        let mutable seen: Map<string, string> = Map.ofList []
        let mutable dups: string array array = [||]
        for path in paths do
            let content: string = fs.[path] |> unbox<string>
            if Map.containsKey content seen then
                dups <- Array.append dups [|[|seen.[content] |> unbox<string>; path|]|]
            else
                seen <- Map.add content path seen
        __ret <- dups
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable fs: Map<string, string> = Map.ofList [("a.txt", "hello"); ("b.txt", "world"); ("c.txt", "hello"); ("d.txt", "foo"); ("e.txt", "world")]
        let paths: string array = [|"a.txt"; "b.txt"; "c.txt"; "d.txt"; "e.txt"|]
        let dups: string array array = findDuplicates fs paths
        for pair in dups do
            printfn "%s" (((pair.[0]) + " <==> ") + (pair.[1]))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
