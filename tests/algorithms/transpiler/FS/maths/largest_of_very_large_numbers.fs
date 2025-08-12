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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec ln (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let t: float = (x - 1.0) / (x + 1.0)
        let mutable term: float = t
        let mutable sum: float = 0.0
        let mutable k: int = 1
        while k <= 99 do
            sum <- sum + (term / (float k))
            term <- (term * t) * t
            k <- k + 2
        __ret <- 2.0 * sum
        raise Return
        __ret
    with
        | Return -> __ret
and log10 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (ln (x)) / (ln (10.0))
        raise Return
        __ret
    with
        | Return -> __ret
and absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and res (x: int) (y: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable y = y
    try
        if x = 0 then
            __ret <- 0.0
            raise Return
        if y = 0 then
            __ret <- 1.0
            raise Return
        if x < 0 then
            failwith ("math domain error")
        __ret <- (float y) * (log10 (float x))
        raise Return
        __ret
    with
        | Return -> __ret
and test_res () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (absf ((res (5) (7)) - 4.892790030352132)) > 0.0000001 then
            failwith ("res(5,7) failed")
        if (res (0) (5)) <> 0.0 then
            failwith ("res(0,5) failed")
        if (res (3) (0)) <> 1.0 then
            failwith ("res(3,0) failed")
        __ret
    with
        | Return -> __ret
and compare (x1: int) (y1: int) (x2: int) (y2: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x1 = x1
    let mutable y1 = y1
    let mutable x2 = x2
    let mutable y2 = y2
    try
        let r1: float = res (x1) (y1)
        let r2: float = res (x2) (y2)
        if r1 > r2 then
            __ret <- (("Largest number is " + (_str (x1))) + " ^ ") + (_str (y1))
            raise Return
        if r2 > r1 then
            __ret <- (("Largest number is " + (_str (x2))) + " ^ ") + (_str (y2))
            raise Return
        __ret <- "Both are equal"
        raise Return
        __ret
    with
        | Return -> __ret
test_res()
printfn "%s" (compare (5) (7) (4) (8))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
