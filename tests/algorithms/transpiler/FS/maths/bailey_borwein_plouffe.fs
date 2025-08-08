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
let rec mod_pow (``base``: int) (exponent: int) (modulus: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exponent = exponent
    let mutable modulus = modulus
    try
        let mutable result: int = 1
        let mutable b: int = ((``base`` % modulus + modulus) % modulus)
        let mutable e: int = exponent
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                result <- (((result * b) % modulus + modulus) % modulus)
            b <- (((b * b) % modulus + modulus) % modulus)
            e <- _floordiv e 2
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec pow_float (``base``: float) (exponent: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exponent = exponent
    try
        let mutable exp: int = exponent
        let mutable result: float = 1.0
        if exp < 0 then
            exp <- -exp
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        if exponent < 0 then
            result <- 1.0 / result
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec hex_digit (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n < 10 then
            __ret <- _str (n)
            raise Return
        let letters: string array = unbox<string array> [|"a"; "b"; "c"; "d"; "e"; "f"|]
        __ret <- _idx letters (n - 10)
        raise Return
        __ret
    with
        | Return -> __ret
let rec floor_float (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable i: int = int x
        if (float i) > x then
            i <- i - 1
        __ret <- float i
        raise Return
        __ret
    with
        | Return -> __ret
let rec subsum (digit_pos_to_extract: int) (denominator_addend: int) (precision: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable digit_pos_to_extract = digit_pos_to_extract
    let mutable denominator_addend = denominator_addend
    let mutable precision = precision
    try
        let mutable total: float = 0.0
        let mutable sum_index: int = 0
        while sum_index < (digit_pos_to_extract + precision) do
            let denominator: int = (8 * sum_index) + denominator_addend
            if sum_index < digit_pos_to_extract then
                let exponent: int = (digit_pos_to_extract - 1) - sum_index
                let exponential_term: int = mod_pow (16) (exponent) (denominator)
                total <- total + ((float exponential_term) / (float denominator))
            else
                let exponent: int = (digit_pos_to_extract - 1) - sum_index
                let exponential_term: float = pow_float (16.0) (exponent)
                total <- total + (exponential_term / (float denominator))
            sum_index <- sum_index + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
let rec bailey_borwein_plouffe (digit_position: int) (precision: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable digit_position = digit_position
    let mutable precision = precision
    try
        if digit_position <= 0 then
            failwith ("Digit position must be a positive integer")
        if precision < 0 then
            failwith ("Precision must be a nonnegative integer")
        let sum_result: float = (((4.0 * (subsum (digit_position) (1) (precision))) - (2.0 * (subsum (digit_position) (4) (precision)))) - (1.0 * (subsum (digit_position) (5) (precision)))) - (1.0 * (subsum (digit_position) (6) (precision)))
        let fraction: float = sum_result - (floor_float (sum_result))
        let digit: int = int (fraction * 16.0)
        let hd: string = hex_digit (digit)
        __ret <- hd
        raise Return
        __ret
    with
        | Return -> __ret
let mutable digits: string = ""
let mutable i: int = 1
while i <= 10 do
    digits <- digits + (bailey_borwein_plouffe (i) (1000))
    i <- i + 1
printfn "%s" (digits)
printfn "%s" (bailey_borwein_plouffe (5) (10000))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
