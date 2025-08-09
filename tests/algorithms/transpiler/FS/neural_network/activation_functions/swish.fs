// Generated 2025-08-09 10:14 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec exp_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable sum: float = 1.0
        let mutable term: float = 1.0
        let mutable i: int = 1
        while i <= 20 do
            term <- (term * x) / (float i)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and sigmoid (vector: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable vector = vector
    try
        let mutable result: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (vector)) do
            let v: float = _idx vector (int i)
            let s: float = 1.0 / (1.0 + (exp_approx (-v)))
            result <- Array.append result [|s|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and swish (vector: float array) (beta: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable vector = vector
    let mutable beta = beta
    try
        let mutable result: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (vector)) do
            let v: float = _idx vector (int i)
            let s: float = 1.0 / (1.0 + (exp_approx ((-beta) * v)))
            result <- Array.append result [|(v * s)|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and sigmoid_linear_unit (vector: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable vector = vector
    try
        __ret <- swish (vector) (1.0)
        raise Return
        __ret
    with
        | Return -> __ret
and approx_equal (a: float) (b: float) (eps: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    let mutable eps = eps
    try
        let diff: float = if a > b then (a - b) else (b - a)
        __ret <- diff < eps
        raise Return
        __ret
    with
        | Return -> __ret
and approx_equal_list (a: float array) (b: float array) (eps: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    let mutable eps = eps
    try
        if (Seq.length (a)) <> (Seq.length (b)) then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            if not (approx_equal (_idx a (int i)) (_idx b (int i)) (eps)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and test_swish () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let v: float array = unbox<float array> [|-1.0; 1.0; 2.0|]
        let eps: float = 0.001
        if not (approx_equal_list (sigmoid (v)) (unbox<float array> [|0.26894142; 0.73105858; 0.88079708|]) (eps)) then
            failwith ("sigmoid incorrect")
        if not (approx_equal_list (sigmoid_linear_unit (v)) (unbox<float array> [|-0.26894142; 0.73105858; 1.76159416|]) (eps)) then
            failwith ("sigmoid_linear_unit incorrect")
        if not (approx_equal_list (swish (v) (2.0)) (unbox<float array> [|-0.11920292; 0.88079708; 1.96402758|]) (eps)) then
            failwith ("swish incorrect")
        if not (approx_equal_list (swish (unbox<float array> [|-2.0|]) (1.0)) (unbox<float array> [|-0.23840584|]) (eps)) then
            failwith ("swish with parameter 1 incorrect")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_swish()
        printfn "%s" (_str (sigmoid (unbox<float array> [|-1.0; 1.0; 2.0|])))
        printfn "%s" (_str (sigmoid_linear_unit (unbox<float array> [|-1.0; 1.0; 2.0|])))
        printfn "%s" (_str (swish (unbox<float array> [|-1.0; 1.0; 2.0|]) (2.0)))
        printfn "%s" (_str (swish (unbox<float array> [|-2.0|]) (1.0)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
