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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
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
type Complex = {
    mutable _re: float
    mutable _im: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec c_add (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { _re = (a._re) + (b._re); _im = (a._im) + (b._im) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec c_sub (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { _re = (a._re) - (b._re); _im = (a._im) - (b._im) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec c_mul (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { _re = ((a._re) * (b._re)) - ((a._im) * (b._im)); _im = ((a._re) * (b._im)) + ((a._im) * (b._re)) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec c_mul_scalar (a: Complex) (s: float) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable s = s
    try
        __ret <- { _re = (a._re) * s; _im = (a._im) * s }
        raise Return
        __ret
    with
        | Return -> __ret
let rec c_div_scalar (a: Complex) (s: float) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable s = s
    try
        __ret <- { _re = (a._re) / s; _im = (a._im) / s }
        raise Return
        __ret
    with
        | Return -> __ret
let PI: float = 3.141592653589793
let rec sin_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable i: int = 1
        while i < 10 do
            let k1: float = 2.0 * (float i)
            let k2: float = k1 + 1.0
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec cos_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: int = 1
        while i < 10 do
            let k1: float = (2.0 * (float i)) - 1.0
            let k2: float = 2.0 * (float i)
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec exp_i (theta: float) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable theta = theta
    try
        __ret <- { _re = cos_taylor (theta); _im = sin_taylor (theta) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec make_complex_list (n: int) (value: Complex) =
    let mutable __ret : Complex array = Unchecked.defaultof<Complex array>
    let mutable n = n
    let mutable value = value
    try
        let mutable arr: Complex array = Array.empty<Complex>
        let mutable i: int = 0
        while i < n do
            arr <- Array.append arr [|value|]
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec fft (a: Complex array) (invert: bool) =
    let mutable __ret : Complex array = Unchecked.defaultof<Complex array>
    let mutable a = a
    let mutable invert = invert
    try
        let mutable n: int = Seq.length (a)
        if n = 1 then
            __ret <- unbox<Complex array> [|_idx a (0)|]
            raise Return
        let mutable a0: Complex array = Array.empty<Complex>
        let mutable a1: Complex array = Array.empty<Complex>
        let mutable i: int = 0
        while i < (_floordiv n 2) do
            a0 <- Array.append a0 [|(_idx a (int ((int64 2) * (int64 i))))|]
            a1 <- Array.append a1 [|(_idx a (int (((int64 2) * (int64 i)) + (int64 1))))|]
            i <- i + 1
        let y0: Complex array = fft (a0) (invert)
        let y1: Complex array = fft (a1) (invert)
        let angle: float = ((2.0 * PI) / (float n)) * (if invert then (-1.0) else 1.0)
        let mutable w: Complex = { _re = 1.0; _im = 0.0 }
        let wn: Complex = exp_i (angle)
        let mutable y: Complex array = make_complex_list (n) ({ _re = 0.0; _im = 0.0 })
        i <- 0
        while i < (_floordiv n 2) do
            let t: Complex = c_mul (w) (_idx y1 (i))
            let u: Complex = _idx y0 (i)
            let mutable even: Complex = c_add (u) (t)
            let mutable odd: Complex = c_sub (u) (t)
            if invert then
                even <- c_div_scalar (even) (2.0)
                odd <- c_div_scalar (odd) (2.0)
            y.[i] <- even
            y.[i + (_floordiv n 2)] <- odd
            w <- c_mul (w) (wn)
            i <- i + 1
        __ret <- y
        raise Return
        __ret
    with
        | Return -> __ret
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
let rec round_to (x: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable ndigits = ndigits
    try
        let m: float = pow10 (ndigits)
        __ret <- (floor ((x * m) + 0.5)) / m
        raise Return
        __ret
    with
        | Return -> __ret
let rec list_to_string (l: float array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable l = l
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (l)) do
            s <- s + (_str (_idx l (i)))
            if (i + 1) < (Seq.length (l)) then
                s <- s + ", "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec multiply_poly (a: float array) (b: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable a = a
    let mutable b = b
    try
        let mutable n: int = 1
        while n < (((Seq.length (a)) + (Seq.length (b))) - 1) do
            n <- int ((int64 n) * (int64 2))
        let mutable fa: Complex array = make_complex_list (n) ({ _re = 0.0; _im = 0.0 })
        let mutable fb: Complex array = make_complex_list (n) ({ _re = 0.0; _im = 0.0 })
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            fa.[i] <- { _re = _idx a (i); _im = 0.0 }
            i <- i + 1
        i <- 0
        while i < (Seq.length (b)) do
            fb.[i] <- { _re = _idx b (i); _im = 0.0 }
            i <- i + 1
        fa <- fft (fa) (false)
        fb <- fft (fb) (false)
        i <- 0
        while i < n do
            fa.[i] <- c_mul (_idx fa (i)) (_idx fb (i))
            i <- i + 1
        fa <- fft (fa) (true)
        let mutable res: float array = Array.empty<float>
        i <- 0
        while i < (((Seq.length (a)) + (Seq.length (b))) - 1) do
            let ``val``: Complex = _idx fa (i)
            res <- Array.append res [|(round_to (``val``._re) (8))|]
            i <- i + 1
        while ((Seq.length (res)) > 0) && ((_idx res ((Seq.length (res)) - 1)) = 0.0) do
            res <- Array.sub res 0 (((Seq.length (res)) - 1) - 0)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let A: float array = unbox<float array> [|0.0; 1.0; 0.0; 2.0|]
let B: float array = unbox<float array> [|2.0; 3.0; 4.0; 0.0|]
let product: float array = multiply_poly (A) (B)
printfn "%s" (list_to_string (product))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
