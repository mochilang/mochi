// Generated 2025-08-04 20:03 +0700

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
let rec powInt (b: int) (p: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable b = b
    let mutable p = p
    try
        let mutable r: int = 1
        let mutable i: int = 0
        while i < p do
            r <- r * b
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
let a: int = powInt (5) (powInt (3) (2))
let b: int = powInt (powInt (5) (3)) (2)
let c: int = powInt (5) (powInt (3) (2))
printfn "%s" ("5^3^2   = " + (string (a)))
printfn "%s" ("(5^3)^2 = " + (string (b)))
printfn "%s" ("5^(3^2) = " + (string (c)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
