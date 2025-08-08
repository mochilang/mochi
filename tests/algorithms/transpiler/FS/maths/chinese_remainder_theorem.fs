// Generated 2025-08-08 17:35 +0700

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
type EuclidResult = {
    mutable _x: int
    mutable _y: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec extended_euclid (a: int) (b: int) =
    let mutable __ret : EuclidResult = Unchecked.defaultof<EuclidResult>
    let mutable a = a
    let mutable b = b
    try
        if b = 0 then
            __ret <- { _x = 1; _y = 0 }
            raise Return
        let res: EuclidResult = extended_euclid (b) (((a % b + b) % b))
        let k: int = _floordiv a b
        __ret <- { _x = res._y; _y = (res._x) - (k * (res._y)) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec chinese_remainder_theorem (n1: int) (r1: int) (n2: int) (r2: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n1 = n1
    let mutable r1 = r1
    let mutable n2 = n2
    let mutable r2 = r2
    try
        let res: EuclidResult = extended_euclid (n1) (n2)
        let _x: int = res._x
        let _y: int = res._y
        let m: int = n1 * n2
        let n: int = ((r2 * _x) * n1) + ((r1 * _y) * n2)
        __ret <- ((((((n % m + m) % m)) + m) % m + m) % m)
        raise Return
        __ret
    with
        | Return -> __ret
let rec invert_modulo (a: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable n = n
    try
        let res: EuclidResult = extended_euclid (a) (n)
        let mutable b: int = res._x
        if b < 0 then
            b <- ((((((b % n + n) % n)) + n) % n + n) % n)
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
let rec chinese_remainder_theorem2 (n1: int) (r1: int) (n2: int) (r2: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n1 = n1
    let mutable r1 = r1
    let mutable n2 = n2
    let mutable r2 = r2
    try
        let _x: int = invert_modulo (n1) (n2)
        let _y: int = invert_modulo (n2) (n1)
        let m: int = n1 * n2
        let n: int = ((r2 * _x) * n1) + ((r1 * _y) * n2)
        __ret <- ((((((n % m + m) % m)) + m) % m + m) % m)
        raise Return
        __ret
    with
        | Return -> __ret
let e1: EuclidResult = extended_euclid (10) (6)
printfn "%s" (((_str (e1._x)) + ",") + (_str (e1._y)))
let e2: EuclidResult = extended_euclid (7) (5)
printfn "%s" (((_str (e2._x)) + ",") + (_str (e2._y)))
printfn "%s" (_str (chinese_remainder_theorem (5) (1) (7) (3)))
printfn "%s" (_str (chinese_remainder_theorem (6) (1) (4) (3)))
printfn "%s" (_str (invert_modulo (2) (5)))
printfn "%s" (_str (invert_modulo (8) (7)))
printfn "%s" (_str (chinese_remainder_theorem2 (5) (1) (7) (3)))
printfn "%s" (_str (chinese_remainder_theorem2 (6) (1) (4) (3)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
