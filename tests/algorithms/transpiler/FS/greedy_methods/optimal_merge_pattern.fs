// Generated 2025-08-08 16:03 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec index_of_min (xs: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    try
        let mutable min_idx: int = 0
        let mutable i: int = 1
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) < (_idx xs (min_idx)) then
                min_idx <- i
            i <- i + 1
        __ret <- min_idx
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_at (xs: int array) (idx: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable idx = idx
    try
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i <> idx then
                res <- Array.append res [|(_idx xs (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec optimal_merge_pattern (files: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable files = files
    try
        let mutable arr: int array = files
        let mutable optimal_merge_cost: int = 0
        while (Seq.length (arr)) > 1 do
            let mutable temp: int = 0
            let mutable k: int = 0
            while k < 2 do
                let mutable min_idx: int = index_of_min (arr)
                temp <- temp + (_idx arr (min_idx))
                arr <- remove_at (arr) (min_idx)
                k <- k + 1
            arr <- Array.append arr [|temp|]
            optimal_merge_cost <- optimal_merge_cost + temp
        __ret <- optimal_merge_cost
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (optimal_merge_pattern (unbox<int array> [|2; 3; 4|]))
printfn "%d" (optimal_merge_pattern (unbox<int array> [|5; 10; 20; 30; 30|]))
printfn "%d" (optimal_merge_pattern (unbox<int array> [|8; 8; 8; 8; 8|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
