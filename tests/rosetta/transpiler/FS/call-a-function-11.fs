// Generated 2025-07-27 15:57 +0700

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
let rec zeroval (ival: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ival = ival
    try
        let mutable x: int = ival
        x <- 0
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and zeroptr (ref: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable ref = ref
    try
        ref.[0] <- 0
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable i: int = 1
        printfn "%s" ("initial: " + (string i))
        let tmp: int = zeroval i
        printfn "%s" ("zeroval: " + (string i))
        let mutable box: int array = [|i|]
        zeroptr box
        i <- unbox<int> (box.[0])
        printfn "%s" ("zeroptr: " + (string i))
        printfn "%s" "pointer: 0"
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
