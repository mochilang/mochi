// Generated 2025-08-07 15:46 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec actual_power (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        if b = 0 then
            __ret <- 1
            raise Return
        let half: int = actual_power (a) (b / 2)
        if (((b % 2 + 2) % 2)) = 0 then
            __ret <- half * half
            raise Return
        __ret <- (a * half) * half
        raise Return
        __ret
    with
        | Return -> __ret
let rec power (a: int) (b: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if b < 0 then (1.0 / (1.0 * (float (actual_power (a) (-b))))) else (1.0 * (float (actual_power (a) (b))))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (power (4) (6)))
printfn "%s" (_str (power (2) (3)))
printfn "%s" (_str (power (-2) (3)))
printfn "%s" (_str (power (2) (-3)))
printfn "%s" (_str (power (-2) (-3)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
