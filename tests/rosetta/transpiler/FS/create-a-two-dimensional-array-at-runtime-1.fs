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
let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let row: int = 3
        let col: int = 4
        let mutable a: int array array = [||]
        let mutable i: int = 0
        while i < row do
            let mutable rowArr: int array = [||]
            let mutable j: int = 0
            while j < col do
                rowArr <- Array.append rowArr [|0|]
                j <- j + 1
            a <- Array.append a [|rowArr|]
            i <- i + 1
        printfn "%s" ("a[0][0] = " + (string ((a.[0]).[0])))
        (a.[int (row - 1)]).[int (col - 1)] <- 7
        printfn "%s" ((((("a[" + (string (row - 1))) + "][") + (string (col - 1))) + "] = ") + (string ((a.[int (row - 1)]).[int (col - 1)])))
        a <- unbox<int array array> null
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
