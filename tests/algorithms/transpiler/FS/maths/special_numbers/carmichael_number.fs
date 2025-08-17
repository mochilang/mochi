// Generated 2025-08-17 12:28 +0700

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
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | _ ->
        let s = sprintf "%A" v
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
let rec abs_int (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- if x < 0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a = 0 then (abs_int (b)) else (gcd (((b % a + a) % a)) (a))
        raise Return
        __ret
    with
        | Return -> __ret
and power (x: int) (y: int) (m: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    let mutable y = y
    let mutable m = m
    try
        if y = 0 then
            __ret <- ((1 % m + m) % m)
            raise Return
        let mutable temp: int = (((power (x) (_floordiv (int y) (int 2)) (m)) % m + m) % m)
        temp <- (((temp * temp) % m + m) % m)
        if (((y % 2 + 2) % 2)) = 1 then
            temp <- (((temp * x) % m + m) % m)
        __ret <- temp
        raise Return
        __ret
    with
        | Return -> __ret
and is_carmichael_number (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n <= 0 then
            ignore (failwith ("Number must be positive"))
        let mutable b: int = 2
        while b < n do
            if (gcd (b) (n)) = 1 then
                if (power (b) (n - 1) (n)) <> 1 then
                    __ret <- false
                    raise Return
            b <- b + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (power (2) (15) (3))))
ignore (printfn "%s" (_str (power (5) (1) (30))))
ignore (printfn "%s" (_str (is_carmichael_number (4))))
ignore (printfn "%s" (_str (is_carmichael_number (561))))
ignore (printfn "%s" (_str (is_carmichael_number (562))))
ignore (printfn "%s" (_str (is_carmichael_number (1105))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
