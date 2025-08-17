// Generated 2025-08-17 12:28 +0700

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
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let PI: float = 3.141592653589793
let rec abs (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and floor (x: float) =
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
and pow (x: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable n = n
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < n do
            result <- result * x
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and factorial (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable result: float = 1.0
        let mutable i: int = 2
        while i <= n do
            result <- result * (float i)
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and radians (deg: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable deg = deg
    try
        __ret <- (deg * PI) / 180.0
        raise Return
        __ret
    with
        | Return -> __ret
and taylor_sin (angle_in_degrees: float) (accuracy: int) (rounded_values_count: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable angle_in_degrees = angle_in_degrees
    let mutable accuracy = accuracy
    let mutable rounded_values_count = rounded_values_count
    try
        let k: float = floor (angle_in_degrees / 360.0)
        let mutable angle: float = angle_in_degrees - (k * 360.0)
        let angle_in_radians: float = radians (angle)
        let mutable result: float = angle_in_radians
        let mutable a: int = 3
        let mutable sign: float = -1.0
        let mutable i: int = 0
        while i < accuracy do
            result <- result + ((sign * (pow (angle_in_radians) (a))) / (factorial (a)))
            sign <- -sign
            a <- a + 2
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and test_sin () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let eps: float = 0.0000001
        if (abs ((taylor_sin (0.0) (18) (10)) - 0.0)) > eps then
            ignore (failwith ("sin(0) failed"))
        if (abs ((taylor_sin (90.0) (18) (10)) - 1.0)) > eps then
            ignore (failwith ("sin(90) failed"))
        if (abs ((taylor_sin (180.0) (18) (10)) - 0.0)) > eps then
            ignore (failwith ("sin(180) failed"))
        if (abs ((taylor_sin (270.0) (18) (10)) - (-1.0))) > eps then
            ignore (failwith ("sin(270) failed"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (test_sin())
        let res: float = taylor_sin (64.0) (18) (10)
        ignore (printfn "%s" (_str (res)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
