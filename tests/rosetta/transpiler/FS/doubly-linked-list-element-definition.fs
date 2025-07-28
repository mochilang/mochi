// Generated 2025-07-28 10:03 +0700

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
let rec Node (value: string) (next: obj) (prev: obj) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable value = value
    let mutable next = next
    let mutable prev = prev
    try
        __ret <- unbox<Map<string, obj>> (Map.ofList [("value", box value); ("next", box next); ("prev", box prev)])
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable a: Map<string, obj> = Node "A" null null
        let mutable b: Map<string, obj> = Node "B" null a
        a <- Map.add "next" (box b) a
        let mutable c: Map<string, obj> = Node "C" null b
        b <- Map.add "next" (box c) b
        let mutable p = a
        let mutable line: string = ""
        while p <> null do
            line <- line + (unbox<string> (p.["value"]))
            p <- unbox<map> (p.["next"])
            if p <> null then
                line <- line + " "
        printfn "%s" line
        p <- c
        line <- ""
        while p <> null do
            line <- line + (unbox<string> (p.["value"]))
            p <- unbox<map> (p.["prev"])
            if p <> null then
                line <- line + " "
        printfn "%s" line
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
