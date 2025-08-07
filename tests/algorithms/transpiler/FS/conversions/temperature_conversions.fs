// Generated 2025-08-07 10:31 +0700

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
let rec celsius_to_fahrenheit (c: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable c = c
    let mutable ndigits = ndigits
    try
        __ret <- round_to (((c * 9.0) / 5.0) + 32.0) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec celsius_to_kelvin (c: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable c = c
    let mutable ndigits = ndigits
    try
        __ret <- round_to (c + 273.15) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec celsius_to_rankine (c: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable c = c
    let mutable ndigits = ndigits
    try
        __ret <- round_to (((c * 9.0) / 5.0) + 491.67) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec fahrenheit_to_celsius (f: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable f = f
    let mutable ndigits = ndigits
    try
        __ret <- round_to (((f - 32.0) * 5.0) / 9.0) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec fahrenheit_to_kelvin (f: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable f = f
    let mutable ndigits = ndigits
    try
        __ret <- round_to ((((f - 32.0) * 5.0) / 9.0) + 273.15) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec fahrenheit_to_rankine (f: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable f = f
    let mutable ndigits = ndigits
    try
        __ret <- round_to (f + 459.67) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec kelvin_to_celsius (k: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable k = k
    let mutable ndigits = ndigits
    try
        __ret <- round_to (k - 273.15) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec kelvin_to_fahrenheit (k: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable k = k
    let mutable ndigits = ndigits
    try
        __ret <- round_to ((((k - 273.15) * 9.0) / 5.0) + 32.0) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec kelvin_to_rankine (k: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable k = k
    let mutable ndigits = ndigits
    try
        __ret <- round_to ((k * 9.0) / 5.0) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec rankine_to_celsius (r: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    let mutable ndigits = ndigits
    try
        __ret <- round_to (((r - 491.67) * 5.0) / 9.0) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec rankine_to_fahrenheit (r: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    let mutable ndigits = ndigits
    try
        __ret <- round_to (r - 459.67) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec rankine_to_kelvin (r: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    let mutable ndigits = ndigits
    try
        __ret <- round_to ((r * 5.0) / 9.0) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec reaumur_to_kelvin (r: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    let mutable ndigits = ndigits
    try
        __ret <- round_to ((r * 1.25) + 273.15) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec reaumur_to_fahrenheit (r: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    let mutable ndigits = ndigits
    try
        __ret <- round_to ((r * 2.25) + 32.0) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec reaumur_to_celsius (r: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    let mutable ndigits = ndigits
    try
        __ret <- round_to (r * 1.25) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
let rec reaumur_to_rankine (r: float) (ndigits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    let mutable ndigits = ndigits
    try
        __ret <- round_to (((r * 2.25) + 32.0) + 459.67) (ndigits)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (celsius_to_fahrenheit (0.0) (2))
printfn "%g" (celsius_to_kelvin (0.0) (2))
printfn "%g" (celsius_to_rankine (0.0) (2))
printfn "%g" (fahrenheit_to_celsius (32.0) (2))
printfn "%g" (fahrenheit_to_kelvin (32.0) (2))
printfn "%g" (fahrenheit_to_rankine (32.0) (2))
printfn "%g" (kelvin_to_celsius (273.15) (2))
printfn "%g" (kelvin_to_fahrenheit (273.15) (2))
printfn "%g" (kelvin_to_rankine (273.15) (2))
printfn "%g" (rankine_to_celsius (491.67) (2))
printfn "%g" (rankine_to_fahrenheit (491.67) (2))
printfn "%g" (rankine_to_kelvin (491.67) (2))
printfn "%g" (reaumur_to_kelvin (80.0) (2))
printfn "%g" (reaumur_to_fahrenheit (80.0) (2))
printfn "%g" (reaumur_to_celsius (80.0) (2))
printfn "%g" (reaumur_to_rankine (80.0) (2))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
