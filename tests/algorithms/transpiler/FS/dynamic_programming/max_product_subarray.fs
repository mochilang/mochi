// Generated 2025-08-07 15:46 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec max_product_subarray (numbers: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable numbers = numbers
    try
        if (Seq.length (numbers)) = 0 then
            __ret <- 0
            raise Return
        let mutable max_till_now: int = _idx numbers (0)
        let mutable min_till_now: int = _idx numbers (0)
        let mutable max_prod: int = _idx numbers (0)
        let mutable i: int = 1
        while i < (Seq.length (numbers)) do
            let number: int = _idx numbers (i)
            if number < 0 then
                let temp: int = max_till_now
                max_till_now <- min_till_now
                min_till_now <- temp
            let prod_max: int = max_till_now * number
            if number > prod_max then
                max_till_now <- number
            else
                max_till_now <- prod_max
            let prod_min: int = min_till_now * number
            if number < prod_min then
                min_till_now <- number
            else
                min_till_now <- prod_min
            if max_till_now > max_prod then
                max_prod <- max_till_now
            i <- i + 1
        __ret <- max_prod
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (max_product_subarray (unbox<int array> [|2; 3; -2; 4|]))
printfn "%d" (max_product_subarray (unbox<int array> [|-2; 0; -1|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
