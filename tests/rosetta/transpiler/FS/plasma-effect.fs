// Generated 2025-08-02 10:52 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let PI: float = 3.141592653589793
let rec floorf (x: float) =
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
and frac (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- x - (float (floorf (x)))
        raise Return
        __ret
    with
        | Return -> __ret
and sinApprox (x: float) =
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
and sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= (float 0) then
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
let nframes: int = 10
let w: int = 32
let h: int = 32
let mutable total: int = 0
let mutable f: int = 1
while f <= nframes do
    let mutable y: int = 0
    while y < h do
        let mutable x: int = 0
        while x < w do
            let fx: float = float x
            let fy: float = float y
            let mutable value: float = sinApprox (fx / 16.0)
            value <- value + (float (sinApprox (fy / 8.0)))
            value <- value + (float (sinApprox ((fx + fy) / 16.0)))
            value <- value + (float (sinApprox ((float (sqrtApprox ((fx * fx) + (fy * fy)))) / 8.0)))
            value <- value + 4.0
            value <- value / 8.0
            let rem: float = frac (value + ((float f) / (float nframes)))
            let ci: int = (int ((float nframes) * rem)) + 1
            total <- total + ci
            x <- x + 1
        y <- y + 1
    f <- f + 1
printfn "%d" (total)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
