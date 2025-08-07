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
type SMAValue = {
    value: float
    ok: bool
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec simple_moving_average (data: float array) (window_size: int) =
    let mutable __ret : SMAValue array = Unchecked.defaultof<SMAValue array>
    let mutable data = data
    let mutable window_size = window_size
    try
        if window_size < 1 then
            failwith ("Window size must be a positive integer")
        let mutable result: SMAValue array = [||]
        let mutable window_sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (data)) do
            window_sum <- window_sum + (_idx data (i))
            if i >= window_size then
                window_sum <- window_sum - (_idx data (i - window_size))
            if i >= (window_size - 1) then
                let avg: float = window_sum / (float window_size)
                result <- Array.append result [|{ value = avg; ok = true }|]
            else
                result <- Array.append result [|{ value = 0.0; ok = false }|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let data: float array = [|10.0; 12.0; 15.0; 13.0; 14.0; 16.0; 18.0; 17.0; 19.0; 21.0|]
let window_size: int = 3
let sma_values: SMAValue array = simple_moving_average (data) (window_size)
let mutable idx: int = 0
while idx < (Seq.length (sma_values)) do
    let item: SMAValue = _idx sma_values (idx)
    if item.ok then
        printfn "%s" ((("Day " + (_str (idx + 1))) + ": ") + (_str (item.value)))
    else
        printfn "%s" (("Day " + (_str (idx + 1))) + ": Not enough data for SMA")
    idx <- idx + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
