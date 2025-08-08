// Generated 2025-08-08 18:58 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
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
                if (_idx arr (int j)) > (_idx arr (int (j + 1))) then
                    let temp: int = _idx arr (int j)
                    arr.[int j] <- _idx arr (int (j + 1))
                    arr.[int (j + 1)] <- temp
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec three_sum (nums: int array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable nums = nums
    try
        let sorted: int array = bubble_sort (nums)
        let mutable res: int array array = Array.empty<int array>
        let n: int = Seq.length (sorted)
        let mutable i: int = 0
        while i < (n - 2) do
            if (i = 0) || ((_idx sorted (int i)) <> (_idx sorted (int (i - 1)))) then
                let mutable low: int = i + 1
                let mutable high: int = n - 1
                let c: int = 0 - (_idx sorted (int i))
                while low < high do
                    let s: int = (_idx sorted (int low)) + (_idx sorted (int high))
                    if s = c then
                        let triple: int array = unbox<int array> [|_idx sorted (int i); _idx sorted (int low); _idx sorted (int high)|]
                        res <- Array.append res [|triple|]
                        while (low < high) && ((_idx sorted (int low)) = (_idx sorted (int (low + 1)))) do
                            low <- low + 1
                        while (low < high) && ((_idx sorted (int high)) = (_idx sorted (int (high - 1)))) do
                            high <- high - 1
                        low <- low + 1
                        high <- high - 1
                    else
                        if s < c then
                            low <- low + 1
                        else
                            high <- high - 1
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (three_sum (unbox<int array> [|-1; 0; 1; 2; -1; -4|])))
printfn "%s" (_str (three_sum (unbox<int array> [|1; 2; 3; 4|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
