// Generated 2025-08-11 17:23 +0700

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
let rec quick_sort_3partition (arr: int array) (left: int) (right: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    let mutable left = left
    let mutable right = right
    try
        if right <= left then
            __ret <- arr
            raise Return
        let mutable a: int = left
        let mutable i: int = left
        let mutable b: int = right
        let pivot: int = _idx arr (int left)
        while i <= b do
            if (_idx arr (int i)) < pivot then
                let temp: int = _idx arr (int a)
                arr.[int a] <- _idx arr (int i)
                arr.[int i] <- temp
                a <- a + 1
                i <- i + 1
            else
                if (_idx arr (int i)) > pivot then
                    let temp: int = _idx arr (int b)
                    arr.[int b] <- _idx arr (int i)
                    arr.[int i] <- temp
                    b <- b - 1
                else
                    i <- i + 1
        arr <- quick_sort_3partition (arr) (left) (a - 1)
        arr <- quick_sort_3partition (arr) (b + 1) (right)
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and quick_sort_lomuto_partition (arr: int array) (left: int) (right: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    let mutable left = left
    let mutable right = right
    try
        if left < right then
            let pivot_index: int = lomuto_partition (arr) (left) (right)
            arr <- quick_sort_lomuto_partition (arr) (left) (pivot_index - 1)
            arr <- quick_sort_lomuto_partition (arr) (pivot_index + 1) (right)
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and lomuto_partition (arr: int array) (left: int) (right: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable left = left
    let mutable right = right
    try
        let pivot: int = _idx arr (int right)
        let mutable store_index: int = left
        let mutable i: int = left
        while i < right do
            if (_idx arr (int i)) < pivot then
                let temp: int = _idx arr (int store_index)
                arr.[int store_index] <- _idx arr (int i)
                arr.[int i] <- temp
                store_index <- store_index + 1
            i <- i + 1
        let temp: int = _idx arr (int right)
        arr.[int right] <- _idx arr (int store_index)
        arr.[int store_index] <- temp
        __ret <- store_index
        raise Return
        __ret
    with
        | Return -> __ret
and three_way_radix_quicksort (arr: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    try
        if (Seq.length (arr)) <= 1 then
            __ret <- arr
            raise Return
        let pivot: int = _idx arr (int 0)
        let mutable less: int array = Array.empty<int>
        let mutable equal: int array = Array.empty<int>
        let mutable greater: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            let ``val``: int = _idx arr (int i)
            if ``val`` < pivot then
                less <- Array.append less [|``val``|]
            else
                if ``val`` > pivot then
                    greater <- Array.append greater [|``val``|]
                else
                    equal <- Array.append equal [|``val``|]
            i <- i + 1
        let sorted_less: int array = three_way_radix_quicksort (less)
        let sorted_greater: int array = three_way_radix_quicksort (greater)
        let mutable result: int array = Array.append (sorted_less) (equal)
        result <- Array.append (result) (sorted_greater)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let mutable array1: int array = unbox<int array> [|5; -1; -1; 5; 5; 24; 0|]
array1 <- quick_sort_3partition (array1) (0) ((Seq.length (array1)) - 1)
printfn "%s" (_str (array1))
let mutable array2: int array = unbox<int array> [|9; 0; 2; 6|]
array2 <- quick_sort_3partition (array2) (0) ((Seq.length (array2)) - 1)
printfn "%s" (_str (array2))
let mutable array3: int array = Array.empty<int>
array3 <- quick_sort_3partition (array3) (0) ((Seq.length (array3)) - 1)
printfn "%s" (_str (array3))
let mutable nums1: int array = unbox<int array> [|0; 5; 3; 1; 2|]
nums1 <- quick_sort_lomuto_partition (nums1) (0) ((Seq.length (nums1)) - 1)
printfn "%s" (_str (nums1))
let mutable nums2: int array = Array.empty<int>
nums2 <- quick_sort_lomuto_partition (nums2) (0) ((Seq.length (nums2)) - 1)
printfn "%s" (_str (nums2))
let mutable nums3: int array = unbox<int array> [|-2; 5; 0; -4|]
nums3 <- quick_sort_lomuto_partition (nums3) (0) ((Seq.length (nums3)) - 1)
printfn "%s" (_str (nums3))
printfn "%s" (_str (three_way_radix_quicksort (Array.empty<int>)))
printfn "%s" (_str (three_way_radix_quicksort (unbox<int array> [|1|])))
printfn "%s" (_str (three_way_radix_quicksort (unbox<int array> [|-5; -2; 1; -2; 0; 1|])))
printfn "%s" (_str (three_way_radix_quicksort (unbox<int array> [|1; 2; 5; 1; 2; 0; 0; 5; 2; -1|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
