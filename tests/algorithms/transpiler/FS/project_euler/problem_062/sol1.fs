// Generated 2025-08-22 23:09 +0700

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
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec get_digits (num: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable num = num
    try
        let cube: int64 = ((int64 num) * (int64 num)) * (int64 num)
        let s: string = _str (cube)
        let mutable counts: int array = Array.empty<int>
        let mutable j: int = 0
        while j < 10 do
            counts <- Array.append counts [|0|]
            j <- j + 1
        let mutable i: int = 0
        while i < (String.length (s)) do
            let mutable d: int = int (string (s.[i]))
            counts.[d] <- (_idx counts (int d)) + 1
            i <- i + 1
        let mutable result: string = ""
        let mutable d: int = 0
        while d < 10 do
            let mutable c: int = _idx counts (int d)
            while c > 0 do
                result <- result + (_str (d))
                c <- c - 1
            d <- d + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and solution (max_base: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable max_base = max_base
    try
        let mutable freqs: System.Collections.Generic.IDictionary<string, int array> = _dictCreate []
        let mutable num: int = 0
        while true do
            let digits: string = get_digits (num)
            let mutable arr: int array = Array.empty<int>
            if freqs.ContainsKey(digits) then
                arr <- _dictGet freqs ((string (digits)))
            arr <- Array.append arr [|num|]
            freqs <- _dictAdd (freqs) (string (digits)) (arr)
            if (Seq.length (arr)) = max_base then
                let ``base``: int = _idx arr (int 0)
                __ret <- int (((int64 ``base``) * (int64 ``base``)) * (int64 ``base``))
                raise Return
            num <- num + 1
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" ("solution() = " + (_str (solution (5)))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
