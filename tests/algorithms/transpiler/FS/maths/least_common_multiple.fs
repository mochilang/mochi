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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = if a >= 0 then a else (-a)
        let mutable y: int = if b >= 0 then b else (-b)
        while y <> 0 do
            let temp: int = ((x % y + y) % y)
            x <- y
            y <- temp
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and lcm_slow (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let max: int = if a >= b then a else b
        let mutable multiple: int = max
        while ((((multiple % a + a) % a)) <> 0) || ((((multiple % b + b) % b)) <> 0) do
            multiple <- multiple + max
        __ret <- multiple
        raise Return
        __ret
    with
        | Return -> __ret
and lcm_fast (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- (_floordiv a (gcd (a) (b))) * b
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (lcm_slow (5) (2)))
printfn "%s" (_str (lcm_slow (12) (76)))
printfn "%s" (_str (lcm_fast (5) (2)))
printfn "%s" (_str (lcm_fast (12) (76)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
