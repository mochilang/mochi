// Generated 2025-08-12 07:47 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let rec int_pow (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < exp do
            result <- int ((int64 result) * (int64 ``base``))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and karatsuba (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        if ((String.length (_str (a))) = 1) || ((String.length (_str (b))) = 1) then
            __ret <- int ((int64 a) * (int64 b))
            raise Return
        let mutable m1: int = String.length (_str (a))
        let lb: int = String.length (_str (b))
        if lb > m1 then
            m1 <- lb
        let m2: int = _floordiv m1 2
        let power: int = int_pow (10) (m2)
        let a1: int = _floordiv a power
        let a2: int = ((a % power + power) % power)
        let b1: int = _floordiv b power
        let b2: int = ((b % power + power) % power)
        let x: int = karatsuba (a2) (b2)
        let y: int = karatsuba (a1 + a2) (b1 + b2)
        let z: int = karatsuba (a1) (b1)
        let mutable result: int64 = (((int64 z) * (int64 (int_pow (10) (int ((int64 2) * (int64 m2)))))) + ((int64 ((y - z) - x)) * (int64 power))) + (int64 x)
        __ret <- int result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (_str (karatsuba (15463) (23489)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
