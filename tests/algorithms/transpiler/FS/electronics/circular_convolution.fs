// Generated 2025-08-07 15:46 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec floor (x: float) =
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
let rec pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable p: float = 1.0
        let mutable i: int = 0
        while i < n do
            p <- p * 10.0
            i <- i + 1
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
let rec roundn (x: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable n = n
    try
        let m: float = pow10 (n)
        __ret <- (floor ((x * m) + 0.5)) / m
        raise Return
        __ret
    with
        | Return -> __ret
let rec pad (signal: float array) (target: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable signal = signal
    let mutable target = target
    try
        let mutable s: float array = signal
        while (Seq.length (s)) < target do
            s <- Array.append s [|0.0|]
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec circular_convolution (a: float array) (b: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable a = a
    let mutable b = b
    try
        let n1: int = Seq.length (a)
        let n2: int = Seq.length (b)
        let n: int = if n1 > n2 then n1 else n2
        let mutable x: float array = pad (a) (n)
        let mutable y: float array = pad (b) (n)
        let mutable res: float array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable sum: float = 0.0
            let mutable k: int = 0
            while k < n do
                let j: int = (((i - k) % n + n) % n)
                let idx: int = if j < 0 then (j + n) else j
                sum <- sum + ((_idx x (k)) * (_idx y (idx)))
                k <- k + 1
            res <- Array.append res [|roundn (sum) (2)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let example1: float array = circular_convolution (unbox<float array> [|2.0; 1.0; 2.0; -1.0|]) (unbox<float array> [|1.0; 2.0; 3.0; 4.0|])
printfn "%s" (_str (example1))
let example2: float array = circular_convolution (unbox<float array> [|0.2; 0.4; 0.6; 0.8; 1.0; 1.2; 1.4; 1.6|]) (unbox<float array> [|0.1; 0.3; 0.5; 0.7; 0.9; 1.1; 1.3; 1.5|])
printfn "%s" (_str (example2))
let example3: float array = circular_convolution (unbox<float array> [|-1.0; 1.0; 2.0; -2.0|]) (unbox<float array> [|0.5; 1.0; -1.0; 2.0; 0.75|])
printfn "%s" (_str (example3))
let example4: float array = circular_convolution (unbox<float array> [|1.0; -1.0; 2.0; 3.0; -1.0|]) (unbox<float array> [|1.0; 2.0; 3.0|])
printfn "%s" (_str (example4))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
