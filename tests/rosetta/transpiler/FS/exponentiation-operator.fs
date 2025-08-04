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
let rec printExpI (b: int) (p: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable p = p
    try
        if p < 0 then
            printfn "%s" ((((string (b)) + "^") + (string (p))) + ": negative power not allowed")
            __ret <- ()
            raise Return
        let mutable r: int = 1
        let mutable i: int = 1
        while i <= p do
            r <- r * b
            i <- i + 1
        printfn "%s" (((((string (b)) + "^") + (string (p))) + ": ") + (string (r)))
        __ret
    with
        | Return -> __ret
and expF (b: float) (p: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable b = b
    let mutable p = p
    try
        let mutable neg: bool = false
        if p < 0 then
            neg <- true
            p <- -p
        let mutable r: float = 1.0
        let mutable pow: float = b
        while p > 0 do
            if (((p % 2 + 2) % 2)) = 1 then
                r <- r * pow
            pow <- pow * pow
            p <- p / 2
        if neg then
            r <- 1.0 / r
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and printExpF (b: float) (p: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable p = p
    try
        if (b = 0.0) && (p < 0) then
            printfn "%s" ((((string (b)) + "^") + (string (p))) + ": +Inf")
            __ret <- ()
            raise Return
        printfn "%s" (((((string (b)) + "^") + (string (p))) + ": ") + (string (expF (b) (p))))
        __ret
    with
        | Return -> __ret
printfn "%s" ("expI tests")
printExpI (2) (10)
printExpI (2) (-10)
printExpI (-2) (10)
printExpI (-2) (11)
printExpI (11) (0)
printfn "%s" ("overflow undetected")
printExpI (10) (10)
printfn "%s" ("\nexpF tests:")
printExpF (2.0) (10)
printExpF (2.0) (-10)
printExpF (-2.0) (10)
printExpF (-2.0) (11)
printExpF (11.0) (0)
printfn "%s" ("disallowed in expI, allowed here")
printExpF (0.0) (-1)
printfn "%s" ("other interesting cases for 32 bit float type")
printExpF (10.0) (39)
printExpF (10.0) (-39)
printExpF (-10.0) (39)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
