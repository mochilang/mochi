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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec round_int (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- int (x + 0.5)
        raise Return
        __ret
    with
        | Return -> __ret
let rec rgb_to_cmyk (r_input: int) (g_input: int) (b_input: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable r_input = r_input
    let mutable g_input = g_input
    let mutable b_input = b_input
    try
        if (((((r_input < 0) || (r_input >= 256)) || (g_input < 0)) || (g_input >= 256)) || (b_input < 0)) || (b_input >= 256) then
            failwith ("Expected int of the range 0..255")
        let r: float = (float r_input) / 255.0
        let g: float = (float g_input) / 255.0
        let b: float = (float b_input) / 255.0
        let mutable max_val: float = r
        if g > max_val then
            max_val <- g
        if b > max_val then
            max_val <- b
        let k_float: float = 1.0 - max_val
        if k_float = 1.0 then
            __ret <- unbox<int array> [|0; 0; 0; 100|]
            raise Return
        let c_float: float = (100.0 * ((1.0 - r) - k_float)) / (1.0 - k_float)
        let m_float: float = (100.0 * ((1.0 - g) - k_float)) / (1.0 - k_float)
        let y_float: float = (100.0 * ((1.0 - b) - k_float)) / (1.0 - k_float)
        let k_percent: float = 100.0 * k_float
        let c: int = round_int (c_float)
        let m: int = round_int (m_float)
        let y: int = round_int (y_float)
        let k: int = round_int (k_percent)
        __ret <- unbox<int array> [|c; m; y; k|]
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_repr (rgb_to_cmyk (255) (255) (255)))
printfn "%s" (_repr (rgb_to_cmyk (128) (128) (128)))
printfn "%s" (_repr (rgb_to_cmyk (0) (0) (0)))
printfn "%s" (_repr (rgb_to_cmyk (255) (0) (0)))
printfn "%s" (_repr (rgb_to_cmyk (0) (255) (0)))
printfn "%s" (_repr (rgb_to_cmyk (0) (0) (255)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
