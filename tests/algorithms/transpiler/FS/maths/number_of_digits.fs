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
let rec abs_int (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- if n < 0 then (-n) else n
        raise Return
        __ret
    with
        | Return -> __ret
and num_digits (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable x: int = abs_int (n)
        let mutable digits: int = 1
        while x >= 10 do
            x <- _floordiv x 10
            digits <- digits + 1
        __ret <- digits
        raise Return
        __ret
    with
        | Return -> __ret
and num_digits_fast (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable x: int = abs_int (n)
        let mutable digits: int = 1
        let mutable power: int = 10
        while x >= power do
            power <- int ((int64 power) * (int64 10))
            digits <- digits + 1
        __ret <- digits
        raise Return
        __ret
    with
        | Return -> __ret
and num_digits_faster (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let s: string = _str (abs_int (n))
        __ret <- String.length (s)
        raise Return
        __ret
    with
        | Return -> __ret
and test_num_digits () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (num_digits (12345)) <> 5 then
            failwith ("num_digits 12345 failed")
        if (num_digits (123)) <> 3 then
            failwith ("num_digits 123 failed")
        if (num_digits (0)) <> 1 then
            failwith ("num_digits 0 failed")
        if (num_digits (-1)) <> 1 then
            failwith ("num_digits -1 failed")
        if (num_digits (-123456)) <> 6 then
            failwith ("num_digits -123456 failed")
        if (num_digits_fast (12345)) <> 5 then
            failwith ("num_digits_fast 12345 failed")
        if (num_digits_fast (123)) <> 3 then
            failwith ("num_digits_fast 123 failed")
        if (num_digits_fast (0)) <> 1 then
            failwith ("num_digits_fast 0 failed")
        if (num_digits_fast (-1)) <> 1 then
            failwith ("num_digits_fast -1 failed")
        if (num_digits_fast (-123456)) <> 6 then
            failwith ("num_digits_fast -123456 failed")
        if (num_digits_faster (12345)) <> 5 then
            failwith ("num_digits_faster 12345 failed")
        if (num_digits_faster (123)) <> 3 then
            failwith ("num_digits_faster 123 failed")
        if (num_digits_faster (0)) <> 1 then
            failwith ("num_digits_faster 0 failed")
        if (num_digits_faster (-1)) <> 1 then
            failwith ("num_digits_faster -1 failed")
        if (num_digits_faster (-123456)) <> 6 then
            failwith ("num_digits_faster -123456 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_num_digits()
        printfn "%s" (_str (num_digits (12345)))
        printfn "%s" (_str (num_digits_fast (12345)))
        printfn "%s" (_str (num_digits_faster (12345)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
