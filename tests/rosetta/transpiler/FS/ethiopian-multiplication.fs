// Generated 2025-08-03 16:23 +0700

exception Return
let mutable __ret = ()

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
let rec halve (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    try
        __ret <- i / 2
        raise Return
        __ret
    with
        | Return -> __ret
and double (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    try
        __ret <- i * 2
        raise Return
        __ret
    with
        | Return -> __ret
and isEven (i: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable i = i
    try
        __ret <- (((i % 2 + 2) % 2)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and ethMulti (i: int) (j: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    let mutable j = j
    try
        let mutable r: int = 0
        let mutable x: int = i
        let mutable y: int = j
        while x > 0 do
            if not (isEven (x)) then
                r <- r + y
            x <- halve (x)
            y <- double (y)
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" ("17 ethiopian 34 = " + (string (ethMulti (17) (34))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
