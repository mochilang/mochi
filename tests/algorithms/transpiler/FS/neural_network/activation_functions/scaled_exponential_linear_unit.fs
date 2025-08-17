// Generated 2025-08-17 13:19 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec exp (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- System.Math.Exp(x)
        raise Return
        __ret
    with
        | Return -> __ret
and scaled_exponential_linear_unit (vector: float array) (alpha: float) (lambda_: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable vector = vector
    let mutable alpha = alpha
    let mutable lambda_ = lambda_
    try
        let mutable result: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (vector)) do
            let x: float = _idx vector (int i)
            let y: float = if x > 0.0 then (lambda_ * x) else ((lambda_ * alpha) * ((exp (x)) - 1.0))
            result <- Array.append result [|y|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_repr (scaled_exponential_linear_unit (unbox<float array> [|1.3; 3.7; 2.4|]) (1.6732) (1.0507))))
ignore (printfn "%s" (_repr (scaled_exponential_linear_unit (unbox<float array> [|1.3; 4.7; 8.2|]) (1.6732) (1.0507))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
