// Generated 2025-08-09 10:14 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
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
let rec index_of (xs: int array) (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (int i)) = x then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- 0 - 1
        raise Return
        __ret
    with
        | Return -> __ret
and majority_vote (votes: int array) (votes_needed_to_win: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable votes = votes
    let mutable votes_needed_to_win = votes_needed_to_win
    try
        if votes_needed_to_win < 2 then
            __ret <- Array.empty<int>
            raise Return
        let mutable candidates: int array = Array.empty<int>
        let mutable counts: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (votes)) do
            let v: int = _idx votes (int i)
            let idx: int = index_of (candidates) (v)
            if idx <> (0 - 1) then
                counts.[int idx] <- (_idx counts (int idx)) + 1
            else
                if (Seq.length (candidates)) < (votes_needed_to_win - 1) then
                    candidates <- Array.append candidates [|v|]
                    counts <- Array.append counts [|1|]
                else
                    let mutable j: int = 0
                    while j < (Seq.length (counts)) do
                        counts.[int j] <- (_idx counts (int j)) - 1
                        j <- j + 1
                    let mutable new_candidates: int array = Array.empty<int>
                    let mutable new_counts: int array = Array.empty<int>
                    j <- 0
                    while j < (Seq.length (candidates)) do
                        if (_idx counts (int j)) > 0 then
                            new_candidates <- Array.append new_candidates [|(_idx candidates (int j))|]
                            new_counts <- Array.append new_counts [|(_idx counts (int j))|]
                        j <- j + 1
                    candidates <- new_candidates
                    counts <- new_counts
            i <- i + 1
        let mutable final_counts: int array = Array.empty<int>
        let mutable j: int = 0
        while j < (Seq.length (candidates)) do
            final_counts <- Array.append final_counts [|0|]
            j <- j + 1
        i <- 0
        while i < (Seq.length (votes)) do
            let v: int = _idx votes (int i)
            let idx: int = index_of (candidates) (v)
            if idx <> (0 - 1) then
                final_counts.[int idx] <- (_idx final_counts (int idx)) + 1
            i <- i + 1
        let mutable result: int array = Array.empty<int>
        j <- 0
        while j < (Seq.length (candidates)) do
            if ((int64 (_idx final_counts (int j))) * (int64 votes_needed_to_win)) > (int64 (Seq.length (votes))) then
                result <- Array.append result [|(_idx candidates (int j))|]
            j <- j + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let votes: int array = unbox<int array> [|1; 2; 2; 3; 1; 3; 2|]
        printfn "%s" (_str (majority_vote (votes) (3)))
        printfn "%s" (_str (majority_vote (votes) (2)))
        printfn "%s" (_str (majority_vote (votes) (4)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
