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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let PI: float = 3.141592653589793
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable guess: float = x / 2.0
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec atanApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x > 1.0 then
            __ret <- (PI / 2.0) - (x / ((x * x) + 0.28))
            raise Return
        if x < (-1.0) then
            __ret <- ((-PI) / 2.0) - (x / ((x * x) + 0.28))
            raise Return
        __ret <- x / (1.0 + ((0.28 * x) * x))
        raise Return
        __ret
    with
        | Return -> __ret
let rec atan2Approx (y: float) (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y = y
    let mutable x = x
    try
        if x > 0.0 then
            let r: float = atanApprox (y / x)
            __ret <- r
            raise Return
        if x < 0.0 then
            if y >= 0.0 then
                __ret <- (atanApprox (y / x)) + PI
                raise Return
            __ret <- (atanApprox (y / x)) - PI
            raise Return
        if y > 0.0 then
            __ret <- PI / 2.0
            raise Return
        if y < 0.0 then
            __ret <- (-PI) / 2.0
            raise Return
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec deg (rad: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable rad = rad
    try
        __ret <- (rad * 180.0) / PI
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
let rec round (x: float) (n: int) =
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
let rec rectangular_to_polar (real: float) (img: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable real = real
    let mutable img = img
    try
        let ``mod``: float = round (sqrtApprox ((real * real) + (img * img))) (2)
        let ang: float = round (deg (atan2Approx (img) (real))) (2)
        __ret <- unbox<float array> [|``mod``; ang|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec show (real: float) (img: float) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable real = real
    let mutable img = img
    try
        let r: float array = rectangular_to_polar (real) (img)
        printfn "%s" (_str (r))
        __ret
    with
        | Return -> __ret
show (5.0) (-5.0)
show (-1.0) (1.0)
show (-1.0) (-1.0)
show (0.0000000001) (0.0000000001)
show (-0.0000000001) (0.0000000001)
show (9.75) (5.93)
show (10000.0) (99999.0)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
