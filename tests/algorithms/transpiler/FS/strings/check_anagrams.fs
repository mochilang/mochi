// Generated 2025-08-11 17:23 +0700

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
open System.Collections.Generic

let rec strip_and_remove_spaces (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable start: int = 0
        let mutable ``end``: int = (String.length (s)) - 1
        while (start < (String.length (s))) && ((string (s.[start])) = " ") do
            start <- start + 1
        while (``end`` >= start) && ((string (s.[``end``])) = " ") do
            ``end`` <- ``end`` - 1
        let mutable res: string = ""
        let mutable i: int = start
        while i <= ``end`` do
            let ch: string = string (s.[i])
            if ch <> " " then
                res <- res + ch
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and check_anagrams (a: string) (b: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        let mutable s1: string = a.ToLower()
        let mutable s2: string = b.ToLower()
        s1 <- strip_and_remove_spaces (s1)
        s2 <- strip_and_remove_spaces (s2)
        if (String.length (s1)) <> (String.length (s2)) then
            __ret <- false
            raise Return
        let mutable count: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let mutable i: int = 0
        while i < (String.length (s1)) do
            let c1: string = string (s1.[i])
            let c2: string = string (s2.[i])
            if count.ContainsKey(c1) then
                count.[c1] <- (_dictGet count ((string (c1)))) + 1
            else
                count.[c1] <- 1
            if count.ContainsKey(c2) then
                count.[c2] <- (_dictGet count ((string (c2)))) - 1
            else
                count.[c2] <- -1
            i <- i + 1
        for ch in count.Keys do
            if (_dictGet count ((string (ch)))) <> 0 then
                __ret <- false
                raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and print_bool (b: bool) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    try
        if b then
            printfn "%b" (true)
        else
            printfn "%b" (false)
        __ret
    with
        | Return -> __ret
print_bool (check_anagrams ("Silent") ("Listen"))
print_bool (check_anagrams ("This is a string") ("Is this a string"))
print_bool (check_anagrams ("This is    a      string") ("Is     this a string"))
print_bool (check_anagrams ("There") ("Their"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
