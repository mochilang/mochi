// Generated 2025-08-12 13:41 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
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
open System.Collections.Generic

let DIGIT_FACTORIAL: int array = unbox<int array> [|1; 1; 2; 6; 24; 120; 720; 5040; 40320; 362880|]
let rec digit_factorial_sum (number: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    try
        if number < 0 then
            ignore (failwith ("Parameter number must be greater than or equal to 0"))
        if number = 0 then
            __ret <- _idx DIGIT_FACTORIAL (int 0)
            raise Return
        let mutable n: int = number
        let mutable total: int = 0
        while n > 0 do
            let digit: int = ((n % 10 + 10) % 10)
            total <- total + (_idx DIGIT_FACTORIAL (int digit))
            n <- _floordiv n 10
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and chain_len (n: int) (limit: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    let mutable limit = limit
    try
        let mutable seen: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let mutable length: int = 0
        let mutable cur: int = n
        while ((seen.ContainsKey(cur)) = false) && (length <= limit) do
            seen <- _dictAdd (seen) (cur) (true)
            length <- length + 1
            cur <- digit_factorial_sum (cur)
        __ret <- length
        raise Return
        __ret
    with
        | Return -> __ret
and solution (chain_length: int) (number_limit: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable chain_length = chain_length
    let mutable number_limit = number_limit
    try
        if (chain_length <= 0) || (number_limit <= 0) then
            ignore (failwith ("Parameters chain_length and number_limit must be greater than 0"))
        let mutable count: int = 0
        let mutable start: int = 1
        while start < number_limit do
            if (chain_len (start) (chain_length)) = chain_length then
                count <- count + 1
            start <- start + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (solution (60) (1000000))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
