// Generated 2025-08-08 18:09 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
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
let rec ``mod`` (a: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable n = n
    try
        let r: int = ((a % n + n) % n)
        if r < 0 then
            __ret <- r + n
            raise Return
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and greatest_common_divisor (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = if a < 0 then (-a) else a
        let mutable y: int = if b < 0 then (-b) else b
        while y <> 0 do
            let t: int = ((x % y + y) % y)
            x <- y
            y <- t
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and extended_gcd (a: int) (b: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        if b = 0 then
            __ret <- unbox<int array> [|a; 1; 0|]
            raise Return
        let res: int array = extended_gcd (b) (((a % b + b) % b))
        let d: int = _idx res (0)
        let p: int = _idx res (1)
        let q: int = _idx res (2)
        let mutable x: int = q
        let mutable y: int64 = (int64 p) - ((int64 q) * (int64 (_floordiv a b)))
        __ret <- Array.map int [|int64 (d); int64 (x); y|]
        raise Return
        __ret
    with
        | Return -> __ret
and extended_euclid (a: int) (b: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        if b = 0 then
            __ret <- unbox<int array> [|1; 0|]
            raise Return
        let res: int array = extended_euclid (b) (((a % b + b) % b))
        let mutable x: int = _idx res (1)
        let mutable y: int64 = (int64 (_idx res (0))) - ((int64 (_floordiv a b)) * (int64 (_idx res (1))))
        __ret <- Array.map int [|int64 (x); y|]
        raise Return
        __ret
    with
        | Return -> __ret
and invert_modulo (a: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable n = n
    try
        let res: int array = extended_euclid (a) (n)
        let inv: int = _idx res (0)
        __ret <- int (``mod`` (inv) (n))
        raise Return
        __ret
    with
        | Return -> __ret
and modular_division (a: int) (b: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    let mutable n = n
    try
        if n <= 1 then
            failwith ("n must be > 1")
        if a <= 0 then
            failwith ("a must be > 0")
        if (greatest_common_divisor (a) (n)) <> 1 then
            failwith ("gcd(a,n) != 1")
        let eg: int array = extended_gcd (n) (a)
        let s: int = _idx eg (2)
        __ret <- int (``mod`` (int ((int64 b) * (int64 s))) (n))
        raise Return
        __ret
    with
        | Return -> __ret
and modular_division2 (a: int) (b: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    let mutable n = n
    try
        let s: int = invert_modulo (a) (n)
        __ret <- int (``mod`` (int ((int64 b) * (int64 s))) (n))
        raise Return
        __ret
    with
        | Return -> __ret
and tests () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (modular_division (4) (8) (5)) <> 2 then
            failwith ("md1")
        if (modular_division (3) (8) (5)) <> 1 then
            failwith ("md2")
        if (modular_division (4) (11) (5)) <> 4 then
            failwith ("md3")
        if (modular_division2 (4) (8) (5)) <> 2 then
            failwith ("md21")
        if (modular_division2 (3) (8) (5)) <> 1 then
            failwith ("md22")
        if (modular_division2 (4) (11) (5)) <> 4 then
            failwith ("md23")
        if (invert_modulo (2) (5)) <> 3 then
            failwith ("inv")
        let eg: int array = extended_gcd (10) (6)
        if (((_idx eg (0)) <> 2) || ((_idx eg (1)) <> (-1))) || ((_idx eg (2)) <> 2) then
            failwith ("eg")
        let eu: int array = extended_euclid (10) (6)
        if ((_idx eu (0)) <> (-1)) || ((_idx eu (1)) <> 2) then
            failwith ("eu")
        if (greatest_common_divisor (121) (11)) <> 11 then
            failwith ("gcd")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        tests()
        printfn "%s" (_str (modular_division (4) (8) (5)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
