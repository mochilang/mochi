// Generated 2025-08-11 16:20 +0700

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec binary_search (arr: int array) (item: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable arr = arr
    let mutable item = item
    try
        let mutable low: int = 0
        let mutable high: int = (Seq.length (arr)) - 1
        while low <= high do
            let mid: int = _floordiv (low + high) 2
            let ``val``: int = _idx arr (int mid)
            if ``val`` = item then
                __ret <- true
                raise Return
            if item < ``val`` then
                high <- mid - 1
            else
                low <- mid + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let mutable arr1: int array = unbox<int array> [|0; 1; 2; 8; 13; 17; 19; 32; 42|]
printfn "%b" (binary_search (arr1) (3))
printfn "%b" (binary_search (arr1) (13))
let mutable arr2: int array = unbox<int array> [|4; 4; 5; 6; 7|]
printfn "%b" (binary_search (arr2) (4))
printfn "%b" (binary_search (arr2) (-10))
let mutable arr3: int array = unbox<int array> [|-18; 2|]
printfn "%b" (binary_search (arr3) (-18))
let mutable arr4: int array = unbox<int array> [|5|]
printfn "%b" (binary_search (arr4) (5))
let mutable arr5: int array = Array.empty<int>
printfn "%b" (binary_search (arr5) (1))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
