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
let rec bubble_sort (nums: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable nums = nums
    try
        let mutable arr: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (nums)) do
            arr <- Array.append arr [|(_idx nums (int i))|]
            i <- i + 1
        let mutable n: int = Seq.length (arr)
        let mutable a: int = 0
        while a < n do
            let mutable b: int = 0
            while b < ((n - a) - 1) do
                if (_idx arr (int b)) > (_idx arr (int (b + 1))) then
                    let tmp: int = _idx arr (int b)
                    arr.[int b] <- _idx arr (int (b + 1))
                    arr.[int (b + 1)] <- tmp
                b <- b + 1
            a <- a + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and sort3 (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable arr: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            arr <- Array.append arr [|(_idx xs (int i))|]
            i <- i + 1
        let mutable n: int = Seq.length (arr)
        let mutable a: int = 0
        while a < n do
            let mutable b: int = 0
            while b < ((n - a) - 1) do
                if (_idx arr (int b)) > (_idx arr (int (b + 1))) then
                    let tmp: int = _idx arr (int b)
                    arr.[int b] <- _idx arr (int (b + 1))
                    arr.[int (b + 1)] <- tmp
                b <- b + 1
            a <- a + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and triplet_sum1 (arr: int array) (target: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    let mutable target = target
    try
        let mutable i: int = 0
        while i < ((Seq.length (arr)) - 2) do
            let mutable j: int = i + 1
            while j < ((Seq.length (arr)) - 1) do
                let mutable k: int = j + 1
                while k < (Seq.length (arr)) do
                    if (((_idx arr (int i)) + (_idx arr (int j))) + (_idx arr (int k))) = target then
                        __ret <- sort3 (unbox<int array> [|_idx arr (int i); _idx arr (int j); _idx arr (int k)|])
                        raise Return
                    k <- k + 1
                j <- j + 1
            i <- i + 1
        __ret <- unbox<int array> [|0; 0; 0|]
        raise Return
        __ret
    with
        | Return -> __ret
and triplet_sum2 (arr: int array) (target: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    let mutable target = target
    try
        let sorted: int array = bubble_sort (arr)
        let n: int = Seq.length (sorted)
        let mutable i: int = 0
        while i < (n - 2) do
            let mutable left: int = i + 1
            let mutable right: int = n - 1
            while left < right do
                let s: int = ((_idx sorted (int i)) + (_idx sorted (int left))) + (_idx sorted (int right))
                if s = target then
                    __ret <- unbox<int array> [|_idx sorted (int i); _idx sorted (int left); _idx sorted (int right)|]
                    raise Return
                if s < target then
                    left <- left + 1
                else
                    right <- right - 1
            i <- i + 1
        __ret <- unbox<int array> [|0; 0; 0|]
        raise Return
        __ret
    with
        | Return -> __ret
and list_equal (a: int array) (b: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        if (Seq.length (a)) <> (Seq.length (b)) then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            if (_idx a (int i)) <> (_idx b (int i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and test_triplet_sum () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let arr1: int array = unbox<int array> [|13; 29; 7; 23; 5|]
        if not (list_equal (triplet_sum1 (arr1) (35)) (unbox<int array> [|5; 7; 23|])) then
            failwith ("ts1 case1 failed")
        if not (list_equal (triplet_sum2 (arr1) (35)) (unbox<int array> [|5; 7; 23|])) then
            failwith ("ts2 case1 failed")
        let arr2: int array = unbox<int array> [|37; 9; 19; 50; 44|]
        if not (list_equal (triplet_sum1 (arr2) (65)) (unbox<int array> [|9; 19; 37|])) then
            failwith ("ts1 case2 failed")
        if not (list_equal (triplet_sum2 (arr2) (65)) (unbox<int array> [|9; 19; 37|])) then
            failwith ("ts2 case2 failed")
        let arr3: int array = unbox<int array> [|6; 47; 27; 1; 15|]
        if not (list_equal (triplet_sum1 (arr3) (11)) (unbox<int array> [|0; 0; 0|])) then
            failwith ("ts1 case3 failed")
        if not (list_equal (triplet_sum2 (arr3) (11)) (unbox<int array> [|0; 0; 0|])) then
            failwith ("ts2 case3 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_triplet_sum()
        let sample: int array = unbox<int array> [|13; 29; 7; 23; 5|]
        let res: int array = triplet_sum2 (sample) (35)
        printfn "%s" (((((_str (_idx res (int 0))) + " ") + (_str (_idx res (int 1)))) + " ") + (_str (_idx res (int 2))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
