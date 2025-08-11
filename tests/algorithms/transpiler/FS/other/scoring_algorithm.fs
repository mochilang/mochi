// Generated 2025-08-09 23:14 +0700

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
let rec get_data (source_data: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable source_data = source_data
    try
        let mutable data_lists: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (Seq.length (source_data)) do
            let row: float array = _idx source_data (int i)
            let mutable j: int = 0
            while j < (Seq.length (row)) do
                if (Seq.length (data_lists)) < (j + 1) then
                    let mutable empty: float array = Array.empty<float>
                    data_lists <- Array.append data_lists [|empty|]
                data_lists.[int j] <- Array.append (_idx data_lists (int j)) [|(_idx row (int j))|]
                j <- j + 1
            i <- i + 1
        __ret <- data_lists
        raise Return
        __ret
    with
        | Return -> __ret
let rec calculate_each_score (data_lists: float array array) (weights: int array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable data_lists = data_lists
    let mutable weights = weights
    try
        let mutable score_lists: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (Seq.length (data_lists)) do
            let dlist: float array = _idx data_lists (int i)
            let weight: int = _idx weights (int i)
            let mutable mind: float = _idx dlist (int 0)
            let mutable maxd: float = _idx dlist (int 0)
            let mutable j: int = 1
            while j < (Seq.length (dlist)) do
                let ``val``: float = _idx dlist (int j)
                if ``val`` < mind then
                    mind <- ``val``
                if ``val`` > maxd then
                    maxd <- ``val``
                j <- j + 1
            let mutable score: float array = Array.empty<float>
            j <- 0
            if weight = 0 then
                while j < (Seq.length (dlist)) do
                    let item: float = _idx dlist (int j)
                    if (maxd - mind) = 0.0 then
                        score <- Array.append score [|1.0|]
                    else
                        score <- Array.append score [|(1.0 - ((item - mind) / (maxd - mind)))|]
                    j <- j + 1
            else
                while j < (Seq.length (dlist)) do
                    let item: float = _idx dlist (int j)
                    if (maxd - mind) = 0.0 then
                        score <- Array.append score [|0.0|]
                    else
                        score <- Array.append score [|((item - mind) / (maxd - mind))|]
                    j <- j + 1
            score_lists <- Array.append score_lists [|score|]
            i <- i + 1
        __ret <- score_lists
        raise Return
        __ret
    with
        | Return -> __ret
let rec generate_final_scores (score_lists: float array array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable score_lists = score_lists
    try
        let count: int = Seq.length (_idx score_lists (int 0))
        let mutable final_scores: float array = Array.empty<float>
        let mutable i: int = 0
        while i < count do
            final_scores <- Array.append final_scores [|0.0|]
            i <- i + 1
        i <- 0
        while i < (Seq.length (score_lists)) do
            let slist: float array = _idx score_lists (int i)
            let mutable j: int = 0
            while j < (Seq.length (slist)) do
                final_scores.[int j] <- (_idx final_scores (int j)) + (_idx slist (int j))
                j <- j + 1
            i <- i + 1
        __ret <- final_scores
        raise Return
        __ret
    with
        | Return -> __ret
let rec procentual_proximity (source_data: float array array) (weights: int array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable source_data = source_data
    let mutable weights = weights
    try
        let mutable data_lists: float array array = get_data (source_data)
        let mutable score_lists: float array array = calculate_each_score (data_lists) (weights)
        let mutable final_scores: float array = generate_final_scores (score_lists)
        let mutable i: int = 0
        while i < (Seq.length (final_scores)) do
            source_data.[int i] <- Array.append (_idx source_data (int i)) [|(_idx final_scores (int i))|]
            i <- i + 1
        __ret <- source_data
        raise Return
        __ret
    with
        | Return -> __ret
let mutable vehicles: float array array = Array.empty<float array>
vehicles <- Array.append vehicles [|[|20.0; 60.0; 2012.0|]|]
vehicles <- Array.append vehicles [|[|23.0; 90.0; 2015.0|]|]
vehicles <- Array.append vehicles [|[|22.0; 50.0; 2011.0|]|]
let mutable weights: int array = unbox<int array> [|0; 0; 1|]
let result: float array array = procentual_proximity (vehicles) (weights)
printfn "%s" (_str (result))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
