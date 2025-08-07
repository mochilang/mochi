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
type Result = {
    start: int
    ``end``: int
    sum: float
}
let rec max_cross_sum (arr: float array) (low: int) (mid: int) (high: int) =
    let mutable __ret : Result = Unchecked.defaultof<Result>
    let mutable arr = arr
    let mutable low = low
    let mutable mid = mid
    let mutable high = high
    try
        let mutable left_sum: float = -1000000000000000000.0
        let mutable max_left: int = -1
        let mutable sum: float = 0.0
        let mutable i: int = mid
        while i >= low do
            sum <- sum + (_idx arr (i))
            if sum > left_sum then
                left_sum <- sum
                max_left <- i
            i <- i - 1
        let mutable right_sum: float = -1000000000000000000.0
        let mutable max_right: int = -1
        sum <- 0.0
        i <- mid + 1
        while i <= high do
            sum <- sum + (_idx arr (i))
            if sum > right_sum then
                right_sum <- sum
                max_right <- i
            i <- i + 1
        __ret <- { start = max_left; ``end`` = max_right; sum = left_sum + right_sum }
        raise Return
        __ret
    with
        | Return -> __ret
and max_subarray (arr: float array) (low: int) (high: int) =
    let mutable __ret : Result = Unchecked.defaultof<Result>
    let mutable arr = arr
    let mutable low = low
    let mutable high = high
    try
        if (Seq.length (arr)) = 0 then
            __ret <- { start = -1; ``end`` = -1; sum = 0.0 }
            raise Return
        if low = high then
            __ret <- { start = low; ``end`` = high; sum = _idx arr (low) }
            raise Return
        let mid: int = (low + high) / 2
        let left: Result = max_subarray (arr) (low) (mid)
        let right: Result = max_subarray (arr) (mid + 1) (high)
        let cross: Result = max_cross_sum (arr) (low) (mid) (high)
        if ((left.sum) >= (right.sum)) && ((left.sum) >= (cross.sum)) then
            __ret <- left
            raise Return
        if ((right.sum) >= (left.sum)) && ((right.sum) >= (cross.sum)) then
            __ret <- right
            raise Return
        __ret <- cross
        raise Return
        __ret
    with
        | Return -> __ret
and show (res: Result) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable res = res
    try
        printfn "%s" (((((("[" + (_str (res.start))) + ", ") + (_str (res.``end``))) + ", ") + (_str (res.sum))) + "]")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let nums1: float array = [|-2.0; 1.0; -3.0; 4.0; -1.0; 2.0; 1.0; -5.0; 4.0|]
        let res1: Result = max_subarray (nums1) (0) ((Seq.length (nums1)) - 1)
        show (res1)
        let nums2: float array = [|2.0; 8.0; 9.0|]
        let res2: Result = max_subarray (nums2) (0) ((Seq.length (nums2)) - 1)
        show (res2)
        let nums3: float array = [|0.0; 0.0|]
        let res3: Result = max_subarray (nums3) (0) ((Seq.length (nums3)) - 1)
        show (res3)
        let nums4: float array = [|-1.0; 0.0; 1.0|]
        let res4: Result = max_subarray (nums4) (0) ((Seq.length (nums4)) - 1)
        show (res4)
        let nums5: float array = [|-2.0; -3.0; -1.0; -4.0; -6.0|]
        let res5: Result = max_subarray (nums5) (0) ((Seq.length (nums5)) - 1)
        show (res5)
        let nums6: float array = [||]
        let res6: Result = max_subarray (nums6) (0) (0)
        show (res6)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
