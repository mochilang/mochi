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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec fibNumber () =
    let mutable __ret : unit -> int = Unchecked.defaultof<unit -> int>
    try
        let mutable a: int = 0
        let mutable b: int = 1
        __ret <- unbox<unit -> int> (        fun () -> 
            let tmp: int = a + b
            a <- b
            b <- tmp
            __ret <- unbox<unit -> int> a
            raise Return)
        raise Return
        __ret
    with
        | Return -> __ret
let rec fibSequence (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let f: unit -> int = fibNumber()
        let mutable r: int = 0
        let mutable i: int = 0
        while i < n do
            r <- int (f())
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
