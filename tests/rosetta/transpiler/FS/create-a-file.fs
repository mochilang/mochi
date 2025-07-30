// Generated 2025-07-30 21:41 +0700

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
let rec createFile (fs: Map<string, bool>) (fn: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable fs = fs
    let mutable fn = fn
    try
        if Map.containsKey fn fs then
            printfn "%s" (("open " + fn) + ": file exists")
        else
            fs <- Map.add fn false fs
            printfn "%s" (("file " + fn) + " created!")
        __ret
    with
        | Return -> __ret
and createDir (fs: Map<string, bool>) (dn: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable fs = fs
    let mutable dn = dn
    try
        if Map.containsKey dn fs then
            printfn "%s" (("mkdir " + dn) + ": file exists")
        else
            fs <- Map.add dn true fs
            printfn "%s" (("directory " + dn) + " created!")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable fs: Map<string, bool> = Map.ofList []
        fs <- Map.add "docs" true fs
        createFile fs "input.txt"
        createFile fs "/input.txt"
        createDir fs "docs"
        createDir fs "/docs"
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
