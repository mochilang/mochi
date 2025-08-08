// Generated 2025-08-08 17:07 +0700

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec bubble_sort (nums: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable nums = nums
    try
        let mutable arr: int array = nums
        let mutable n: int = Seq.length (arr)
        let mutable i: int = 0
        while i < n do
            let mutable j: int = 0
            while j < (n - 1) do
                let a: int = _idx arr (j)
                let b: int = _idx arr (j + 1)
                if a > b then
                    arr.[j] <- b
                    arr.[j + 1] <- a
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec median (nums: int array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable nums = nums
    try
        let sorted_list: int array = bubble_sort (nums)
        let length: int = Seq.length (sorted_list)
        let mid_index: int = _floordiv length 2
        if (((length % 2 + 2) % 2)) = 0 then
            __ret <- (float ((_idx sorted_list (mid_index)) + (_idx sorted_list (mid_index - 1)))) / 2.0
            raise Return
        else
            __ret <- float (_idx sorted_list (mid_index))
            raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (median (unbox<int array> [|0|])))
printfn "%s" (_str (median (unbox<int array> [|4; 1; 3; 2|])))
printfn "%s" (_str (median (unbox<int array> [|2; 70; 6; 50; 20; 8; 4|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
