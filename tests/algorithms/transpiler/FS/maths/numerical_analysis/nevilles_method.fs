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
type NevilleResult = {
    mutable _value: float
    mutable _table: float array array
}
let rec neville_interpolate (x_points: float array) (y_points: float array) (x0: float) =
    let mutable __ret : NevilleResult = Unchecked.defaultof<NevilleResult>
    let mutable x_points = x_points
    let mutable y_points = y_points
    let mutable x0 = x0
    try
        let n: int = Seq.length (x_points)
        let mutable q: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < n do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|0.0|]
                j <- j + 1
            q <- Array.append q [|row|]
            i <- i + 1
        i <- 0
        while i < n do
            q.[i].[1] <- _idx y_points (i)
            i <- i + 1
        let mutable col: int = 2
        while col < n do
            let mutable row_idx: int = col
            while row_idx < n do
                q.[row_idx].[col] <- (((x0 - (_idx x_points ((row_idx - col) + 1))) * (_idx (_idx q (row_idx)) (col - 1))) - ((x0 - (_idx x_points (row_idx))) * (_idx (_idx q (row_idx - 1)) (col - 1)))) / ((_idx x_points (row_idx)) - (_idx x_points ((row_idx - col) + 1)))
                row_idx <- row_idx + 1
            col <- col + 1
        __ret <- { _value = _idx (_idx q (n - 1)) (n - 1); _table = q }
        raise Return
        __ret
    with
        | Return -> __ret
and test_neville () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let xs: float array = unbox<float array> [|1.0; 2.0; 3.0; 4.0; 6.0|]
        let ys: float array = unbox<float array> [|6.0; 7.0; 8.0; 9.0; 11.0|]
        let r1: NevilleResult = neville_interpolate (xs) (ys) (5.0)
        if (r1._value) <> 10.0 then
            failwith ("neville_interpolate at 5 failed")
        let r2: NevilleResult = neville_interpolate (xs) (ys) (99.0)
        if (r2._value) <> 104.0 then
            failwith ("neville_interpolate at 99 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_neville()
        let xs: float array = unbox<float array> [|1.0; 2.0; 3.0; 4.0; 6.0|]
        let ys: float array = unbox<float array> [|6.0; 7.0; 8.0; 9.0; 11.0|]
        let r: NevilleResult = neville_interpolate (xs) (ys) (5.0)
        printfn "%g" (r._value)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
