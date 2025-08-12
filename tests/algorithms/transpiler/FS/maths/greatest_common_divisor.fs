// Generated 2025-08-12 08:17 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec abs_int (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- if n < 0 then (-n) else n
        raise Return
        __ret
    with
        | Return -> __ret
and greatest_common_divisor (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let x: int = abs_int (a)
        let y: int = abs_int (b)
        if x = 0 then
            __ret <- y
            raise Return
        __ret <- greatest_common_divisor (((y % x + x) % x)) (x)
        raise Return
        __ret
    with
        | Return -> __ret
and gcd_by_iterative (x: int) (y: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    let mutable y = y
    try
        let mutable a: int = abs_int (x)
        let mutable b: int = abs_int (y)
        while b <> 0 do
            let temp: int = b
            b <- ((a % b + b) % b)
            a <- temp
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (greatest_common_divisor (24) (40)))
printfn "%s" (_str (greatest_common_divisor (1) (1)))
printfn "%s" (_str (greatest_common_divisor (1) (800)))
printfn "%s" (_str (greatest_common_divisor (11) (37)))
printfn "%s" (_str (greatest_common_divisor (3) (5)))
printfn "%s" (_str (greatest_common_divisor (16) (4)))
printfn "%s" (_str (greatest_common_divisor (-3) (9)))
printfn "%s" (_str (greatest_common_divisor (9) (-3)))
printfn "%s" (_str (greatest_common_divisor (3) (-9)))
printfn "%s" (_str (greatest_common_divisor (-3) (-9)))
printfn "%s" (_str (gcd_by_iterative (24) (40)))
printfn "%s" (_str ((greatest_common_divisor (24) (40)) = (gcd_by_iterative (24) (40))))
printfn "%s" (_str (gcd_by_iterative (-3) (-9)))
printfn "%s" (_str (gcd_by_iterative (3) (-9)))
printfn "%s" (_str (gcd_by_iterative (1) (-800)))
printfn "%s" (_str (gcd_by_iterative (11) (37)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
