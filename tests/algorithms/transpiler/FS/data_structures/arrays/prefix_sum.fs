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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type PrefixSum = {
    prefix_sum: int array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_prefix_sum (arr: int array) =
    let mutable __ret : PrefixSum = Unchecked.defaultof<PrefixSum>
    let mutable arr = arr
    try
        let mutable prefix: int array = [||]
        let mutable running: int = 0
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            running <- running + (_idx arr (i))
            prefix <- Array.append prefix [|running|]
            i <- i + 1
        __ret <- { prefix_sum = prefix }
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_sum (ps: PrefixSum) (start: int) (``end``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ps = ps
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        let mutable prefix: int array = ps.prefix_sum
        if (Seq.length (prefix)) = 0 then
            failwith ("The array is empty.")
        if ((start < 0) || (``end`` >= (Seq.length (prefix)))) || (start > ``end``) then
            failwith ("Invalid range specified.")
        if start = 0 then
            __ret <- _idx prefix (``end``)
            raise Return
        __ret <- (_idx prefix (``end``)) - (_idx prefix (start - 1))
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains_sum (ps: PrefixSum) (target_sum: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ps = ps
    let mutable target_sum = target_sum
    try
        let mutable prefix: int array = ps.prefix_sum
        let mutable sums: int array = [|0|]
        let mutable i: int = 0
        while i < (Seq.length (prefix)) do
            let sum_item: int = _idx prefix (i)
            let mutable j: int = 0
            while j < (Seq.length (sums)) do
                if (_idx sums (j)) = (sum_item - target_sum) then
                    __ret <- true
                    raise Return
                j <- j + 1
            sums <- Array.append sums [|sum_item|]
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let ps: PrefixSum = make_prefix_sum (unbox<int array> [|1; 2; 3|])
printfn "%s" (_str (get_sum (ps) (0) (2)))
printfn "%s" (_str (get_sum (ps) (1) (2)))
printfn "%s" (_str (get_sum (ps) (2) (2)))
printfn "%s" (_str (contains_sum (ps) (6)))
printfn "%s" (_str (contains_sum (ps) (5)))
printfn "%s" (_str (contains_sum (ps) (3)))
printfn "%s" (_str (contains_sum (ps) (4)))
printfn "%s" (_str (contains_sum (ps) (7)))
let ps2: PrefixSum = make_prefix_sum (unbox<int array> [|1; -2; 3|])
printfn "%s" (_str (contains_sum (ps2) (2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
