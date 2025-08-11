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
let rec swap (xs: float array) (i: int) (j: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    let mutable i = i
    let mutable j = j
    try
        let mutable res: float array = Array.empty<float>
        let mutable k: int = 0
        while k < (Seq.length (xs)) do
            if k = i then
                res <- Array.append res [|(_idx xs (int j))|]
            else
                if k = j then
                    res <- Array.append res [|(_idx xs (int i))|]
                else
                    res <- Array.append res [|(_idx xs (int k))|]
            k <- k + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and wiggle_sort (nums: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable nums = nums
    try
        let mutable i: int = 0
        let mutable res: float array = nums
        while i < (Seq.length (res)) do
            let j: int = if i = 0 then ((Seq.length (res)) - 1) else (i - 1)
            let prev: float = _idx res (int j)
            let curr: float = _idx res (int i)
            if ((((i % 2 + 2) % 2)) = 1) = (prev > curr) then
                res <- swap (res) (j) (i)
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (wiggle_sort (unbox<float array> [|3.0; 5.0; 2.0; 1.0; 6.0; 4.0|])))
printfn "%s" (_str (wiggle_sort (unbox<float array> [|0.0; 5.0; 3.0; 2.0; 2.0|])))
printfn "%s" (_str (wiggle_sort (Array.empty<float>)))
printfn "%s" (_str (wiggle_sort (unbox<float array> [|-2.0; -5.0; -45.0|])))
printfn "%s" (_str (wiggle_sort (unbox<float array> [|-2.1; -5.68; -45.11|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
