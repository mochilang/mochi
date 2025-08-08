// Generated 2025-08-08 18:09 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec perfect_cube (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        let mutable m: int = n
        if m < 0 then
            m <- -m
        let mutable i: int = 0
        while (((int64 i) * (int64 i)) * (int64 i)) < (int64 m) do
            i <- i + 1
        __ret <- (((int64 i) * (int64 i)) * (int64 i)) = (int64 m)
        raise Return
        __ret
    with
        | Return -> __ret
let rec perfect_cube_binary_search (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        let mutable m: int = n
        if m < 0 then
            m <- -m
        let mutable left: int = 0
        let mutable right: int = m
        while left <= right do
            let mid: int = left + (_floordiv (right - left) 2)
            let cube: int64 = ((int64 mid) * (int64 mid)) * (int64 mid)
            if cube = (int64 m) then
                __ret <- true
                raise Return
            if cube < (int64 m) then
                left <- mid + 1
            else
                right <- mid - 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (perfect_cube (27)))
printfn "%s" (_str (perfect_cube (4)))
printfn "%s" (_str (perfect_cube_binary_search (27)))
printfn "%s" (_str (perfect_cube_binary_search (64)))
printfn "%s" (_str (perfect_cube_binary_search (4)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
