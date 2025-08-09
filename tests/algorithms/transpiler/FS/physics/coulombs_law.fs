// Generated 2025-08-09 16:21 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec format2 (x: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    try
        let sign: string = if x < 0.0 then "-" else ""
        let y: float = if x < 0.0 then (-x) else x
        let m: float = 100.0
        let scaled: float = y * m
        let mutable i: int = int scaled
        if (scaled - (float i)) >= 0.5 then
            i <- i + 1
        let int_part: int = _floordiv i 100
        let frac_part: int = ((i % 100 + 100) % 100)
        let mutable frac_str: string = _str (frac_part)
        if frac_part < 10 then
            frac_str <- "0" + frac_str
        __ret <- ((sign + (_str (int_part))) + ".") + frac_str
        raise Return
        __ret
    with
        | Return -> __ret
let K: float = 8987551792.3
let rec coulombs_law (q1: float) (q2: float) (radius: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable q1 = q1
    let mutable q2 = q2
    let mutable radius = radius
    try
        if radius <= 0.0 then
            failwith ("radius must be positive")
        let force: float = ((K * q1) * q2) / (radius * radius)
        __ret <- force
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (format2 (coulombs_law (15.5) (20.0) (15.0)))
printfn "%s" (format2 (coulombs_law (1.0) (15.0) (5.0)))
printfn "%s" (format2 (coulombs_law (20.0) (-50.0) (15.0)))
printfn "%s" (format2 (coulombs_law (-5.0) (-8.0) (10.0)))
printfn "%s" (format2 (coulombs_law (50.0) (100.0) (50.0)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
