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
let rec concat (a: int array) (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        let mutable result: int array = Array.empty<int>
        for x in a do
            result <- Array.append result [|x|]
        for x in b do
            result <- Array.append result [|x|]
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and quick_sort (data: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable data = data
    try
        if (Seq.length (data)) <= 1 then
            __ret <- data
            raise Return
        let pivot: int = _idx data (int 0)
        let mutable left: int array = Array.empty<int>
        let mutable right: int array = Array.empty<int>
        let mutable i: int = 1
        while i < (Seq.length (data)) do
            let e: int = _idx data (int i)
            if e <= pivot then
                left <- Array.append left [|e|]
            else
                right <- Array.append right [|e|]
            i <- i + 1
        let sorted_left: int array = quick_sort (left)
        let sorted_right: int array = quick_sort (right)
        let left_pivot: int array = Array.append sorted_left [|pivot|]
        __ret <- concat (left_pivot) (sorted_right)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (quick_sort (unbox<int array> [|2; 1; 0|])))
printfn "%s" (_str (quick_sort (unbox<int array> [|3; 5; 2; 4; 1|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
