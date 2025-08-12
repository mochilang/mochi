// Generated 2025-08-12 07:47 +0700

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
let rec abs_val (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and pow_float (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and nth_root (value: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable value = value
    let mutable n = n
    try
        if value = 0.0 then
            __ret <- 0.0
            raise Return
        let mutable x: float = value / (float n)
        let mutable i: int = 0
        while i < 20 do
            let num: float = ((float (n - 1)) * x) + (value / (pow_float (x) (n - 1)))
            x <- num / (float n)
            i <- i + 1
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and minkowski_distance (point_a: float array) (point_b: float array) (order: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable point_a = point_a
    let mutable point_b = point_b
    let mutable order = order
    try
        if order < 1 then
            failwith ("The order must be greater than or equal to 1.")
        if (Seq.length (point_a)) <> (Seq.length (point_b)) then
            failwith ("Both points must have the same dimension.")
        let mutable total: float = 0.0
        let mutable idx: int = 0
        while idx < (Seq.length (point_a)) do
            let diff: float = abs_val ((_idx point_a (int idx)) - (_idx point_b (int idx)))
            total <- total + (pow_float (diff) (order))
            idx <- idx + 1
        __ret <- nth_root (total) (order)
        raise Return
        __ret
    with
        | Return -> __ret
and test_minkowski () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (abs_val ((minkowski_distance (unbox<float array> [|1.0; 1.0|]) (unbox<float array> [|2.0; 2.0|]) (1)) - 2.0)) > 0.0001 then
            failwith ("minkowski_distance test1 failed")
        if (abs_val ((minkowski_distance (unbox<float array> [|1.0; 2.0; 3.0; 4.0|]) (unbox<float array> [|5.0; 6.0; 7.0; 8.0|]) (2)) - 8.0)) > 0.0001 then
            failwith ("minkowski_distance test2 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_minkowski()
        printfn "%g" (minkowski_distance (unbox<float array> [|5.0|]) (unbox<float array> [|0.0|]) (3))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
