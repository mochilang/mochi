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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec minimum_subarray_sum (target: int) (numbers: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable target = target
    let mutable numbers = numbers
    try
        let n: int = Seq.length (numbers)
        if n = 0 then
            __ret <- 0
            raise Return
        if target = 0 then
            let mutable i: int = 0
            while i < n do
                if (_idx numbers (i)) = 0 then
                    __ret <- 0
                    raise Return
                i <- i + 1
        let mutable left: int = 0
        let mutable right: int = 0
        let mutable curr_sum: int = 0
        let mutable min_len: int = n + 1
        while right < n do
            curr_sum <- curr_sum + (_idx numbers (right))
            while (curr_sum >= target) && (left <= right) do
                let current_len: int = (right - left) + 1
                if current_len < min_len then
                    min_len <- current_len
                curr_sum <- curr_sum - (_idx numbers (left))
                left <- left + 1
            right <- right + 1
        if min_len = (n + 1) then
            __ret <- 0
            raise Return
        __ret <- min_len
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (minimum_subarray_sum (7) (unbox<int array> [|2; 3; 1; 2; 4; 3|])))
printfn "%s" (_str (minimum_subarray_sum (7) (unbox<int array> [|2; 3; -1; 2; 4; -3|])))
printfn "%s" (_str (minimum_subarray_sum (11) (unbox<int array> [|1; 1; 1; 1; 1; 1; 1; 1|])))
printfn "%s" (_str (minimum_subarray_sum (0) (unbox<int array> [|1; 2; 3|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
