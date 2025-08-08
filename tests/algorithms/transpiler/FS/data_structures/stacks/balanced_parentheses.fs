// Generated 2025-08-08 11:10 +0700

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
open System.Collections.Generic

let rec pop_last (xs: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    try
        let mutable res: string array = [||]
        let mutable i: int = 0
        while i < ((Seq.length (xs)) - 1) do
            res <- Array.append res [|(_idx xs (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec balanced_parentheses (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        let mutable stack: string array = [||]
        let pairs: System.Collections.Generic.IDictionary<string, string> = _dictCreate [("(", ")"); ("[", "]"); ("{", "}")]
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = string (s.[i])
            if pairs.ContainsKey(ch) then
                stack <- Array.append stack [|ch|]
            else
                if ((ch = ")") || (ch = "]")) || (ch = "}") then
                    if (Seq.length (stack)) = 0 then
                        __ret <- false
                        raise Return
                    let top: string = _idx stack ((Seq.length (stack)) - 1)
                    if (_dictGet pairs ((string (top)))) <> ch then
                        __ret <- false
                        raise Return
                    stack <- pop_last (stack)
            i <- i + 1
        __ret <- (Seq.length (stack)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
let mutable tests: string array = [|"([]{})"; "[()]{}{[()()]()}"; "[(])"; "1+2*3-4"; ""|]
let mutable idx: int = 0
while idx < (Seq.length (tests)) do
    printfn "%b" (balanced_parentheses (_idx tests (idx)))
    idx <- idx + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
