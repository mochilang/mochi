// Generated 2025-08-07 10:31 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sortFloats (xs: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    try
        let mutable arr: float array = xs
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            let mutable j: int = 0
            while j < ((Seq.length (arr)) - 1) do
                if (_idx arr (j)) > (_idx arr (j + 1)) then
                    let t: float = _idx arr (j)
                    arr.[j] <- _idx arr (j + 1)
                    arr.[j + 1] <- t
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_median_sorted_arrays (nums1: float array) (nums2: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable nums1 = nums1
    let mutable nums2 = nums2
    try
        if ((Seq.length (nums1)) = 0) && ((Seq.length (nums2)) = 0) then
            failwith ("Both input arrays are empty.")
        let mutable merged: float array = [||]
        let mutable i: int = 0
        while i < (Seq.length (nums1)) do
            merged <- Array.append merged [|_idx nums1 (i)|]
            i <- i + 1
        let mutable j: int = 0
        while j < (Seq.length (nums2)) do
            merged <- Array.append merged [|_idx nums2 (j)|]
            j <- j + 1
        let sorted: float array = sortFloats (merged)
        let total: int = Seq.length (sorted)
        if (((total % 2 + 2) % 2)) = 1 then
            __ret <- _idx sorted (total / 2)
            raise Return
        let middle1: float = _idx sorted ((total / 2) - 1)
        let middle2: float = _idx sorted (total / 2)
        __ret <- (middle1 + middle2) / 2.0
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (find_median_sorted_arrays (unbox<float array> [|1.0; 3.0|]) (unbox<float array> [|2.0|]))
printfn "%g" (find_median_sorted_arrays (unbox<float array> [|1.0; 2.0|]) (unbox<float array> [|3.0; 4.0|]))
printfn "%g" (find_median_sorted_arrays (unbox<float array> [|0.0; 0.0|]) (unbox<float array> [|0.0; 0.0|]))
printfn "%g" (find_median_sorted_arrays (Array.empty<float>) (unbox<float array> [|1.0|]))
printfn "%g" (find_median_sorted_arrays (unbox<float array> [|-1000.0|]) (unbox<float array> [|1000.0|]))
printfn "%g" (find_median_sorted_arrays (unbox<float array> [|-1.1; -2.2|]) (unbox<float array> [|-3.3; -4.4|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
