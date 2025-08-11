// Generated 2025-08-09 23:14 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let c: float = 299792458.0
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
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
let rec beta (velocity: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable velocity = velocity
    try
        if velocity > c then
            failwith ("Speed must not exceed light speed 299,792,458 [m/s]!")
        if velocity < 1.0 then
            failwith ("Speed must be greater than or equal to 1!")
        __ret <- velocity / c
        raise Return
        __ret
    with
        | Return -> __ret
let rec gamma (velocity: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable velocity = velocity
    try
        let b: float = beta (velocity)
        __ret <- 1.0 / (sqrtApprox (1.0 - (b * b)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec transformation_matrix (velocity: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable velocity = velocity
    try
        let g: float = gamma (velocity)
        let b: float = beta (velocity)
        __ret <- [|[|g; (-g) * b; 0.0; 0.0|]; [|(-g) * b; g; 0.0; 0.0|]; [|0.0; 0.0; 1.0; 0.0|]; [|0.0; 0.0; 0.0; 1.0|]|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec mat_vec_mul (mat: float array array) (vec: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable mat = mat
    let mutable vec = vec
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < 4 do
            let row: float array = _idx mat (int i)
            let value: float = ((((_idx row (int 0)) * (_idx vec (int 0))) + ((_idx row (int 1)) * (_idx vec (int 1)))) + ((_idx row (int 2)) * (_idx vec (int 2)))) + ((_idx row (int 3)) * (_idx vec (int 3)))
            res <- unbox<float array> (Array.append res [|value|])
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec transform (velocity: float) (event: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable velocity = velocity
    let mutable event = event
    try
        let g: float = gamma (velocity)
        let b: float = beta (velocity)
        let ct: float = (_idx event (int 0)) * c
        let x: float = _idx event (int 1)
        __ret <- unbox<float array> [|(g * ct) - ((g * b) * x); (((-g) * b) * ct) + (g * x); _idx event (int 2); _idx event (int 3)|]
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (beta (c)))
printfn "%s" (_str (beta (199792458.0)))
printfn "%s" (_str (beta (100000.0)))
printfn "%s" (_str (gamma (4.0)))
printfn "%s" (_str (gamma (100000.0)))
printfn "%s" (_str (gamma (30000000.0)))
printfn "%s" (_str (transformation_matrix (29979245.0)))
let v: float array = transform (29979245.0) (unbox<float array> [|1.0; 2.0; 3.0; 4.0|])
printfn "%s" (_str (v))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
