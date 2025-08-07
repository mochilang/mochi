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
let rec partition (arr: int array) (low: int) (high: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable low = low
    let mutable high = high
    try
        let pivot: int = _idx arr (high)
        let mutable i: int = low - 1
        let mutable j: int = low
        while j < high do
            if (_idx arr (j)) >= pivot then
                i <- i + 1
                let mutable tmp: int = _idx arr (i)
                arr.[i] <- _idx arr (j)
                arr.[j] <- tmp
            j <- j + 1
        let mutable k: int = i + 1
        let mutable tmp: int = _idx arr (k)
        arr.[k] <- _idx arr (high)
        arr.[high] <- tmp
        __ret <- k
        raise Return
        __ret
    with
        | Return -> __ret
let rec kth_largest_element (arr: int array) (position: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable position = position
    try
        if (Seq.length (arr)) = 0 then
            __ret <- -1
            raise Return
        if (position < 1) || (position > (Seq.length (arr))) then
            __ret <- -1
            raise Return
        let mutable low: int = 0
        let mutable high: int = (Seq.length (arr)) - 1
        while low <= high do
            if (low > ((Seq.length (arr)) - 1)) || (high < 0) then
                __ret <- -1
                raise Return
            let mutable pivot_index: int = partition (arr) (low) (high)
            if pivot_index = (position - 1) then
                __ret <- _idx arr (pivot_index)
                raise Return
            else
                if pivot_index > (position - 1) then
                    high <- pivot_index - 1
                else
                    low <- pivot_index + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let arr1: int array = [|3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5|]
printfn "%d" (kth_largest_element (arr1) (3))
printfn "%s" ("\n")
let arr2: int array = [|2; 5; 6; 1; 9; 3; 8; 4; 7; 3; 5|]
printfn "%d" (kth_largest_element (arr2) (1))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
