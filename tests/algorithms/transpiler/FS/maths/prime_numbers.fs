// Generated 2025-08-08 18:09 +0700

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
let rec slow_primes (max_n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable max_n = max_n
    try
        let mutable result: int array = Array.empty<int>
        let mutable i: int = 2
        try
            while i <= max_n do
                try
                    let mutable j: int = 2
                    let mutable is_prime: bool = true
                    try
                        while j < i do
                            try
                                if (((i % j + j) % j)) = 0 then
                                    is_prime <- false
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if is_prime then
                        result <- Array.append result [|i|]
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec primes (max_n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable max_n = max_n
    try
        let mutable result: int array = Array.empty<int>
        let mutable i: int = 2
        try
            while i <= max_n do
                try
                    let mutable j: int = 2
                    let mutable is_prime: bool = true
                    try
                        while ((int64 j) * (int64 j)) <= (int64 i) do
                            try
                                if (((i % j + j) % j)) = 0 then
                                    is_prime <- false
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if is_prime then
                        result <- Array.append result [|i|]
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec fast_primes (max_n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable max_n = max_n
    try
        let mutable result: int array = Array.empty<int>
        if max_n >= 2 then
            result <- Array.append result [|2|]
        let mutable i: int = 3
        try
            while i <= max_n do
                try
                    let mutable j: int = 3
                    let mutable is_prime: bool = true
                    try
                        while ((int64 j) * (int64 j)) <= (int64 i) do
                            try
                                if (((i % j + j) % j)) = 0 then
                                    is_prime <- false
                                    raise Break
                                j <- j + 2
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if is_prime then
                        result <- Array.append result [|i|]
                    i <- i + 2
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (slow_primes (25)))
printfn "%s" (_str (primes (25)))
printfn "%s" (_str (fast_primes (25)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
