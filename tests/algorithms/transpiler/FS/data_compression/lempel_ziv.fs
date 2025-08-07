// Generated 2025-08-07 10:31 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec to_binary (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n = 0 then
            __ret <- "0"
            raise Return
        let mutable num: int = n
        let mutable res: string = ""
        while num > 0 do
            let bit: int = ((num % 2 + 2) % 2)
            res <- (_str (bit)) + res
            num <- num / 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains_key_int (m: System.Collections.Generic.IDictionary<string, int>) (key: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable m = m
    let mutable key = key
    try
        for k in m.Keys do
            if (unbox<string> k) = key then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec lzw_compress (bits: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable bits = bits
    try
        let mutable dict: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("0", 0); ("1", 1)]
        let mutable current: string = ""
        let mutable result: string = ""
        let mutable index: int = 2
        let mutable i: int = 0
        while i < (String.length (bits)) do
            let ch: string = string (bits.[i])
            let candidate: string = current + ch
            if contains_key_int (dict) (candidate) then
                current <- candidate
            else
                result <- result + (to_binary (dict.[(string (current))]))
                dict.[candidate] <- index
                index <- index + 1
                current <- ch
            i <- i + 1
        if current <> "" then
            result <- result + (to_binary (dict.[(string (current))]))
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let data: string = "01001100100111"
printfn "%s" (lzw_compress (data))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
