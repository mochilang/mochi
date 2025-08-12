// Generated 2025-08-12 16:24 +0700

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
let rec ln (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            ignore (failwith ("ln domain error"))
        let y: float = (x - 1.0) / (x + 1.0)
        let y2: float = y * y
        let mutable term: float = y
        let mutable sum: float = 0.0
        let mutable k: int = 0
        while k < 10 do
            let denom: float = float ((2 * k) + 1)
            sum <- sum + (term / denom)
            term <- term * y2
            k <- k + 1
        __ret <- 2.0 * sum
        raise Return
        __ret
    with
        | Return -> __ret
and exp (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- System.Math.Exp(x)
        raise Return
        __ret
    with
        | Return -> __ret
and softplus (vector: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable vector = vector
    try
        let mutable result: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (vector)) do
            let x: float = _idx vector (int i)
            let value: float = ln (1.0 + (exp (x)))
            result <- Array.append result [|value|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let v1: float array = unbox<float array> [|2.3; 0.6; -2.0; -3.8|]
        let v2: float array = unbox<float array> [|-9.2; -0.3; 0.45; -4.56|]
        let r1: float array = softplus (v1)
        let r2: float array = softplus (v2)
        ignore (printfn "%s" (_repr (r1)))
        ignore (printfn "%s" (_repr (r2)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
