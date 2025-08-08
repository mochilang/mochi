// Generated 2025-08-08 17:07 +0700

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let PI: float = 3.141592653589793
let rec sinApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable n: int = 1
        while n <= 10 do
            let denom: float = float ((2 * n) * ((2 * n) + 1))
            term <- (((-term) * x) * x) / denom
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec cosApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable n: int = 1
        while n <= 10 do
            let denom: float = float (((2 * n) - 1) * (2 * n))
            term <- (((-term) * x) * x) / denom
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec expApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable sum: float = 1.0
        let mutable term: float = 1.0
        let mutable n: int = 1
        while n < 10 do
            term <- (term * x) / (float n)
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec ln (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let t: float = (x - 1.0) / (x + 1.0)
        let mutable term: float = t
        let mutable sum: float = 0.0
        let mutable n: int = 1
        while n <= 19 do
            sum <- sum + (term / (float n))
            term <- (term * t) * t
            n <- n + 2
        __ret <- 2.0 * sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec log10 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (ln (x)) / (ln (10.0))
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 10 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec normalize (audio: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable audio = audio
    try
        let mutable max_val: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (audio)) do
            let mutable v: float = absf (_idx audio (i))
            if v > max_val then
                max_val <- v
            i <- i + 1
        let mutable res: float array = Array.empty<float>
        i <- 0
        while i < (Seq.length (audio)) do
            res <- Array.append res [|((_idx audio (i)) / max_val)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec dft (frame: float array) (bins: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable frame = frame
    let mutable bins = bins
    try
        let N: int = Seq.length (frame)
        let mutable spec: float array = Array.empty<float>
        let mutable k: int = 0
        while k < bins do
            let mutable real: float = 0.0
            let mutable imag: float = 0.0
            let mutable n: int = 0
            while n < N do
                let angle: float = ((((-2.0) * PI) * (float k)) * (float n)) / (float N)
                real <- real + ((_idx frame (n)) * (cosApprox (angle)))
                imag <- imag + ((_idx frame (n)) * (sinApprox (angle)))
                n <- n + 1
            spec <- Array.append spec [|((real * real) + (imag * imag))|]
            k <- k + 1
        __ret <- spec
        raise Return
        __ret
    with
        | Return -> __ret
let rec triangular_filters (bins: int) (spectrum_size: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable bins = bins
    let mutable spectrum_size = spectrum_size
    try
        let mutable filters: float array array = Array.empty<float array>
        let mutable b: int = 0
        while b < bins do
            let center: int = _floordiv ((b + 1) * spectrum_size) (bins + 1)
            let mutable filt: float array = Array.empty<float>
            let mutable i: int = 0
            while i < spectrum_size do
                let mutable v: float = 0.0
                if i <= center then
                    v <- (float i) / (float center)
                else
                    v <- (float (spectrum_size - i)) / (float (spectrum_size - center))
                filt <- Array.append filt [|v|]
                i <- i + 1
            filters <- Array.append filters [|filt|]
            b <- b + 1
        __ret <- filters
        raise Return
        __ret
    with
        | Return -> __ret
let rec dot (mat: float array array) (vec: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable mat = mat
    let mutable vec = vec
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            let mutable sum: float = 0.0
            let mutable j: int = 0
            while j < (Seq.length (vec)) do
                sum <- sum + ((_idx (_idx mat (i)) (j)) * (_idx vec (j)))
                j <- j + 1
            res <- Array.append res [|sum|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec discrete_cosine_transform (dct_filter_num: int) (filter_num: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable dct_filter_num = dct_filter_num
    let mutable filter_num = filter_num
    try
        let mutable basis: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < dct_filter_num do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < filter_num do
                if i = 0 then
                    row <- Array.append row [|(1.0 / (sqrtApprox (float filter_num)))|]
                else
                    let angle: float = (((float ((2 * j) + 1)) * (float i)) * PI) / (2.0 * (float filter_num))
                    row <- Array.append row [|((cosApprox (angle)) * (sqrtApprox (2.0 / (float filter_num))))|]
                j <- j + 1
            basis <- Array.append basis [|row|]
            i <- i + 1
        __ret <- basis
        raise Return
        __ret
    with
        | Return -> __ret
let rec mfcc (audio: float array) (bins: int) (dct_num: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable audio = audio
    let mutable bins = bins
    let mutable dct_num = dct_num
    try
        let norm: float array = normalize (audio)
        let mutable spec: float array = dft (norm) (bins + 2)
        let mutable filters: float array array = triangular_filters (bins) (Seq.length (spec))
        let energies: float array = dot (filters) (spec)
        let mutable logfb: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (energies)) do
            logfb <- Array.append logfb [|(10.0 * (log10 ((_idx energies (i)) + 0.0000000001)))|]
            i <- i + 1
        let dct_basis: float array array = discrete_cosine_transform (dct_num) (bins)
        let mutable res: float array = dot (dct_basis) (logfb)
        if (Seq.length (res)) = 0 then
            res <- unbox<float array> [|0.0; 0.0; 0.0|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let sample_rate: int = 8000
let size: int = 16
let mutable audio: float array = Array.empty<float>
let mutable n: int = 0
while n < size do
    let t: float = (float n) / (float sample_rate)
    audio <- Array.append audio [|(sinApprox (((2.0 * PI) * 440.0) * t))|]
    n <- n + 1
let coeffs: float array = mfcc (audio) (5) (3)
for c in coeffs do
    printfn "%g" (c)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
