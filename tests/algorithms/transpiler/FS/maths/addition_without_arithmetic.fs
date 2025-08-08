// Generated 2025-08-08 17:07 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let rec _str v =
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
let MAX: int64 = 4294967296L
let HALF: int = int 2147483648L
let rec to_unsigned (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- if n < 0 then (int (MAX + (int64 n))) else n
        raise Return
        __ret
    with
        | Return -> __ret
let rec from_unsigned (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- if n >= HALF then (int ((int64 n) - MAX)) else n
        raise Return
        __ret
    with
        | Return -> __ret
let rec bit_and (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        let mutable res: int = 0
        let mutable bit: int = 1
        let mutable i: int = 0
        while i < 32 do
            if ((((x % 2 + 2) % 2)) = 1) && ((((y % 2 + 2) % 2)) = 1) then
                res <- res + bit
            x <- _floordiv x 2
            y <- _floordiv y 2
            bit <- bit * 2
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec bit_xor (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        let mutable res: int = 0
        let mutable bit: int = 1
        let mutable i: int = 0
        while i < 32 do
            let abit: int = ((x % 2 + 2) % 2)
            let bbit: int = ((y % 2 + 2) % 2)
            if ((((abit + bbit) % 2 + 2) % 2)) = 1 then
                res <- res + bit
            x <- _floordiv x 2
            y <- _floordiv y 2
            bit <- bit * 2
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec lshift1 (num: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    try
        __ret <- int ((((int64 (num * 2)) % MAX + MAX) % MAX))
        raise Return
        __ret
    with
        | Return -> __ret
let rec add (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable first: int = to_unsigned (a)
        let mutable second: int = to_unsigned (b)
        while second <> 0 do
            let carry: int = bit_and (first) (second)
            first <- bit_xor (first) (second)
            second <- lshift1 (carry)
        let result: int = from_unsigned (first)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (add (3) (5)))
printfn "%s" (_str (add (13) (5)))
printfn "%s" (_str (add (-7) (2)))
printfn "%s" (_str (add (0) (-7)))
printfn "%s" (_str (add (-321) (0)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
