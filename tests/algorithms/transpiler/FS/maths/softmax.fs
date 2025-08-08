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
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: int = 1
        while i < 20 do
            term <- (term * x) / (float i)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and softmax (vec: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable vec = vec
    try
        let mutable exps: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (vec)) do
            exps <- Array.append exps [|(exp_approx (_idx vec (int i)))|]
            i <- i + 1
        let mutable total: float = 0.0
        i <- 0
        while i < (Seq.length (exps)) do
            total <- total + (_idx exps (int i))
            i <- i + 1
        let mutable result: float array = Array.empty<float>
        i <- 0
        while i < (Seq.length (exps)) do
            result <- Array.append result [|((_idx exps (int i)) / total)|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and abs_val (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and approx_equal (a: float) (b: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        __ret <- (abs_val (a - b)) < 0.0001
        raise Return
        __ret
    with
        | Return -> __ret
and test_softmax () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let s1: float array = softmax (unbox<float array> [|1.0; 2.0; 3.0; 4.0|])
        let mutable sum1: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (s1)) do
            sum1 <- sum1 + (_idx s1 (int i))
            i <- i + 1
        if not (approx_equal (sum1) (1.0)) then
            failwith ("sum test failed")
        let s2: float array = softmax (unbox<float array> [|5.0; 5.0|])
        if not ((approx_equal (_idx s2 (int 0)) (0.5)) && (approx_equal (_idx s2 (int 1)) (0.5))) then
            failwith ("equal elements test failed")
        let s3: float array = softmax (unbox<float array> [|0.0|])
        if not (approx_equal (_idx s3 (int 0)) (1.0)) then
            failwith ("zero vector test failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_softmax()
        printfn "%s" (_str (softmax (unbox<float array> [|1.0; 2.0; 3.0; 4.0|])))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
