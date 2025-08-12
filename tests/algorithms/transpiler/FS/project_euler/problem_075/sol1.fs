// Generated 2025-08-12 13:41 +0700

exception Break
exception Continue

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        while y <> 0 do
            let t: int = ((x % y + y) % y)
            x <- y
            y <- t
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and solution (limit: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable limit = limit
    try
        let mutable frequencies: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        let mutable m: int = 2
        try
            while ((2 * m) * (m + 1)) <= limit do
                try
                    let mutable n: int = (((m % 2 + 2) % 2)) + 1
                    try
                        while n < m do
                            try
                                if (gcd (m) (n)) > 1 then
                                    n <- n + 2
                                    raise Continue
                                let primitive_perimeter: int = (2 * m) * (m + n)
                                let mutable perimeter: int = primitive_perimeter
                                while perimeter <= limit do
                                    if not (frequencies.ContainsKey(perimeter)) then
                                        frequencies <- _dictAdd (frequencies) (perimeter) (0)
                                    frequencies <- _dictAdd (frequencies) (perimeter) ((_dictGet frequencies (perimeter)) + 1)
                                    perimeter <- perimeter + primitive_perimeter
                                n <- n + 2
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    m <- m + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable count: int = 0
        for p in frequencies.Keys do
            if (_dictGet frequencies (p)) = 1 then
                count <- count + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
let result: int = solution (1500000)
ignore (printfn "%s" ("solution() = " + (_str (result))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
