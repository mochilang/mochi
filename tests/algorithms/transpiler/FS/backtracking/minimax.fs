// Generated 2025-08-06 20:48 +0700

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
let rec minimax (depth: int) (node_index: int) (is_max: bool) (scores: int array) (height: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable depth = depth
    let mutable node_index = node_index
    let mutable is_max = is_max
    let mutable scores = scores
    let mutable height = height
    try
        if depth < 0 then
            failwith ("Depth cannot be less than 0")
        if (Seq.length(scores)) = 0 then
            failwith ("Scores cannot be empty")
        if depth = height then
            __ret <- _idx scores (node_index)
            raise Return
        if is_max then
            let left: int = minimax (depth + 1) (node_index * 2) (false) (scores) (height)
            let right: int = minimax (depth + 1) ((node_index * 2) + 1) (false) (scores) (height)
            if left > right then
                __ret <- left
                raise Return
            else
                __ret <- right
                raise Return
        let left: int = minimax (depth + 1) (node_index * 2) (true) (scores) (height)
        let right: int = minimax (depth + 1) ((node_index * 2) + 1) (true) (scores) (height)
        if left < right then
            __ret <- left
            raise Return
        else
            __ret <- right
            raise Return
        __ret
    with
        | Return -> __ret
and tree_height (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable h: int = 0
        let mutable v: int = n
        while v > 1 do
            v <- v / 2
            h <- h + 1
        __ret <- h
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let scores: int array = [|90; 23; 6; 33; 21; 65; 123; 34423|]
        let height: int = tree_height (Seq.length(scores))
        printfn "%s" ("Optimal value : " + (_str (minimax (0) (0) (true) (scores) (height))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
