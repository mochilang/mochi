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
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec prime_factors (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n <= 0 then
            failwith ("Only positive integers have prime factors")
        let mutable num: int = n
        let mutable pf: int array = Array.empty<int>
        while (((num % 2 + 2) % 2)) = 0 do
            pf <- Array.append pf [|2|]
            num <- _floordiv num 2
        let mutable i: int = 3
        while (i * i) <= num do
            while (((num % i + i) % i)) = 0 do
                pf <- Array.append pf [|i|]
                num <- _floordiv num i
            i <- i + 2
        if num > 2 then
            pf <- Array.append pf [|num|]
        __ret <- pf
        raise Return
        __ret
    with
        | Return -> __ret
let rec number_of_divisors (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n <= 0 then
            failwith ("Only positive numbers are accepted")
        let mutable num: int = n
        let mutable div: int = 1
        let mutable temp: int = 1
        while (((num % 2 + 2) % 2)) = 0 do
            temp <- temp + 1
            num <- _floordiv num 2
        div <- div * temp
        let mutable i: int = 3
        while (i * i) <= num do
            temp <- 1
            while (((num % i + i) % i)) = 0 do
                temp <- temp + 1
                num <- _floordiv num i
            div <- div * temp
            i <- i + 2
        if num > 1 then
            div <- div * 2
        __ret <- div
        raise Return
        __ret
    with
        | Return -> __ret
let rec sum_of_divisors (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n <= 0 then
            failwith ("Only positive numbers are accepted")
        let mutable num: int = n
        let mutable s: int = 1
        let mutable temp: int = 1
        while (((num % 2 + 2) % 2)) = 0 do
            temp <- temp + 1
            num <- _floordiv num 2
        if temp > 1 then
            s <- s * (_floordiv ((pow_int (2) (temp)) - 1) (2 - 1))
        let mutable i: int = 3
        while (i * i) <= num do
            temp <- 1
            while (((num % i + i) % i)) = 0 do
                temp <- temp + 1
                num <- _floordiv num i
            if temp > 1 then
                s <- s * (_floordiv ((pow_int (i) (temp)) - 1) (i - 1))
            i <- i + 2
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains (arr: int array) (x: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable arr = arr
    let mutable x = x
    try
        let mutable idx: int = 0
        while idx < (Seq.length (arr)) do
            if (_idx arr (idx)) = x then
                __ret <- true
                raise Return
            idx <- idx + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec unique (arr: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    try
        let mutable result: int array = Array.empty<int>
        let mutable idx: int = 0
        while idx < (Seq.length (arr)) do
            let v: int = _idx arr (idx)
            if not (contains (result) (v)) then
                result <- Array.append result [|v|]
            idx <- idx + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec euler_phi (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n <= 0 then
            failwith ("Only positive numbers are accepted")
        let mutable s: int = n
        let factors: int array = unique (prime_factors (n))
        let mutable idx: int = 0
        while idx < (Seq.length (factors)) do
            let x: int = _idx factors (idx)
            s <- (_floordiv s x) * (x - 1)
            idx <- idx + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (prime_factors (100)))
printfn "%s" (_str (number_of_divisors (100)))
printfn "%s" (_str (sum_of_divisors (100)))
printfn "%s" (_str (euler_phi (100)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
