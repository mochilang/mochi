// Generated 2025-08-08 18:58 +0700

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
let rec ugly_numbers (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n <= 0 then
            __ret <- 1
            raise Return
        let mutable ugly_nums: int array = Array.empty<int>
        ugly_nums <- Array.append ugly_nums [|1|]
        let mutable i2: int = 0
        let mutable i3: int = 0
        let mutable i5: int = 0
        let mutable next_2: int = 2
        let mutable next_3: int = 3
        let mutable next_5: int = 5
        let mutable count: int = 1
        while count < n do
            let next_num: int = if next_2 < next_3 then (if next_2 < next_5 then next_2 else next_5) else (if next_3 < next_5 then next_3 else next_5)
            ugly_nums <- Array.append ugly_nums [|next_num|]
            if next_num = next_2 then
                i2 <- i2 + 1
                next_2 <- int ((int64 (_idx ugly_nums (int i2))) * (int64 2))
            if next_num = next_3 then
                i3 <- i3 + 1
                next_3 <- int ((int64 (_idx ugly_nums (int i3))) * (int64 3))
            if next_num = next_5 then
                i5 <- i5 + 1
                next_5 <- int ((int64 (_idx ugly_nums (int i5))) * (int64 5))
            count <- count + 1
        __ret <- _idx ugly_nums (int ((Seq.length (ugly_nums)) - 1))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (ugly_numbers (100)))
printfn "%s" (_str (ugly_numbers (0)))
printfn "%s" (_str (ugly_numbers (20)))
printfn "%s" (_str (ugly_numbers (-5)))
printfn "%s" (_str (ugly_numbers (200)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
