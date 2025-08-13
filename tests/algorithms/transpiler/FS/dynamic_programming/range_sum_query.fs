// Generated 2025-08-13 07:12 +0700

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
type Query = {
    mutable _left: int
    mutable _right: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec prefix_sum (arr: int array) (queries: Query array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    let mutable queries = queries
    try
        let mutable dp: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            if i = 0 then
                dp <- Array.append dp [|(_idx arr (int 0))|]
            else
                dp <- Array.append dp [|((_idx dp (int (i - 1))) + (_idx arr (int i)))|]
            i <- i + 1
        let mutable result: int array = Array.empty<int>
        let mutable j: int = 0
        while j < (Seq.length (queries)) do
            let q: Query = _idx queries (int j)
            let mutable sum: int = _idx dp (int (q._right))
            if (q._left) > 0 then
                sum <- sum - (_idx dp (int ((q._left) - 1)))
            result <- Array.append result [|sum|]
            j <- j + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let arr1: int array = unbox<int array> [|1; 4; 6; 2; 61; 12|]
let queries1: Query array = unbox<Query array> [|{ _left = 2; _right = 5 }; { _left = 1; _right = 5 }; { _left = 3; _right = 4 }|]
ignore (printfn "%s" (_str (prefix_sum (arr1) (queries1))))
let arr2: int array = unbox<int array> [|4; 2; 1; 6; 3|]
let queries2: Query array = unbox<Query array> [|{ _left = 3; _right = 4 }; { _left = 1; _right = 3 }; { _left = 0; _right = 2 }|]
ignore (printfn "%s" (_str (prefix_sum (arr2) (queries2))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
