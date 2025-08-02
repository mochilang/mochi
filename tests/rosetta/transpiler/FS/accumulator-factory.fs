// Generated 2025-08-02 14:06 +0700

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
let rec accumulator (sum: obj) =
    let mutable __ret : obj -> obj = Unchecked.defaultof<obj -> obj>
    let mutable sum = sum
    try
        let mutable store: obj array = [|sum|]
        let rec add (nv: obj) =
            let mutable __ret : obj = Unchecked.defaultof<obj>
            let mutable nv = nv
            try
                store.[0] <- box ((System.Convert.ToDouble (store.[0])) + (System.Convert.ToDouble nv))
                __ret <- store.[0]
                raise Return
                __ret
            with
                | Return -> __ret
        __ret <- unbox<obj -> obj> add
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let x: obj -> obj = accumulator (1)
        x (5)
        accumulator (3)
        printfn "%s" (string (x (2.3)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
