// Generated 2025-08-07 16:27 +0700

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
let rec exponential_moving_average (stock_prices: float array) (window_size: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable stock_prices = stock_prices
    let mutable window_size = window_size
    try
        if window_size <= 0 then
            failwith ("window_size must be > 0")
        let alpha: float = 2.0 / (1.0 + (float window_size))
        let mutable moving_average: float = 0.0
        let mutable result: float array = [||]
        let mutable i: int = 0
        while i < (Seq.length (stock_prices)) do
            let price: float = _idx stock_prices (i)
            if i <= window_size then
                if i = 0 then
                    moving_average <- price
                else
                    moving_average <- (moving_average + price) * 0.5
            else
                moving_average <- (alpha * price) + ((1.0 - alpha) * moving_average)
            result <- Array.append result [|moving_average|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let stock_prices: float array = [|2.0; 5.0; 3.0; 8.2; 6.0; 9.0; 10.0|]
let window_size: int = 3
let mutable result: float array = exponential_moving_average (stock_prices) (window_size)
printfn "%s" (_str (result))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
