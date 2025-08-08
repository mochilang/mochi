// Generated 2025-08-08 18:58 +0700

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec pow_int (``base``: int) (exp: int) =
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
let rec armstrong_number (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 1 then
            __ret <- false
            raise Return
        let mutable digits: int = 0
        let mutable temp: int = n
        while temp > 0 do
            temp <- _floordiv temp 10
            digits <- digits + 1
        let mutable total: int = 0
        temp <- n
        while temp > 0 do
            let rem: int = ((temp % 10 + 10) % 10)
            total <- total + (pow_int (rem) (digits))
            temp <- _floordiv temp 10
        __ret <- total = n
        raise Return
        __ret
    with
        | Return -> __ret
let rec pluperfect_number (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 1 then
            __ret <- false
            raise Return
        let mutable digit_histogram: int array = Array.empty<int>
        let mutable i: int = 0
        while i < 10 do
            digit_histogram <- Array.append digit_histogram [|0|]
            i <- i + 1
        let mutable digit_total: int = 0
        let mutable temp: int = n
        while temp > 0 do
            let rem: int = ((temp % 10 + 10) % 10)
            digit_histogram.[int rem] <- (_idx digit_histogram (int rem)) + 1
            digit_total <- digit_total + 1
            temp <- _floordiv temp 10
        let mutable total: int = 0
        i <- 0
        while i < 10 do
            if (_idx digit_histogram (int i)) > 0 then
                total <- int ((int64 total) + ((int64 (_idx digit_histogram (int i))) * (int64 (pow_int (i) (digit_total)))))
            i <- i + 1
        __ret <- total = n
        raise Return
        __ret
    with
        | Return -> __ret
let rec narcissistic_number (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 1 then
            __ret <- false
            raise Return
        let mutable digits: int = 0
        let mutable temp: int = n
        while temp > 0 do
            temp <- _floordiv temp 10
            digits <- digits + 1
        temp <- n
        let mutable total: int = 0
        while temp > 0 do
            let rem: int = ((temp % 10 + 10) % 10)
            total <- total + (pow_int (rem) (digits))
            temp <- _floordiv temp 10
        __ret <- total = n
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%b" (armstrong_number (371))
printfn "%b" (armstrong_number (200))
printfn "%b" (pluperfect_number (371))
printfn "%b" (pluperfect_number (200))
printfn "%b" (narcissistic_number (371))
printfn "%b" (narcissistic_number (200))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
