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
type Query = {
    left: int
    right: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec prefix_sum (arr: int array) (queries: Query array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    let mutable queries = queries
    try
        let mutable dp: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            if i = 0 then
                dp <- Array.append dp [|_idx arr (0)|]
            else
                dp <- Array.append dp [|(_idx dp (i - 1)) + (_idx arr (i))|]
            i <- i + 1
        let mutable result: int array = [||]
        let mutable j: int = 0
        while j < (Seq.length (queries)) do
            let q: Query = _idx queries (j)
            let mutable sum: int = _idx dp (q.right)
            if (q.left) > 0 then
                sum <- sum - (_idx dp ((q.left) - 1))
            result <- Array.append result [|sum|]
            j <- j + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let arr1: int array = [|1; 4; 6; 2; 61; 12|]
let queries1: Query array = [|{ left = 2; right = 5 }; { left = 1; right = 5 }; { left = 3; right = 4 }|]
printfn "%s" (_str (prefix_sum (arr1) (queries1)))
let arr2: int array = [|4; 2; 1; 6; 3|]
let queries2: Query array = [|{ left = 3; right = 4 }; { left = 1; right = 3 }; { left = 0; right = 2 }|]
printfn "%s" (_str (prefix_sum (arr2) (queries2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
