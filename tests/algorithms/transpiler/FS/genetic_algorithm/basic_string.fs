// Generated 2025-08-07 17:32 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type PairString = {
    first: string
    second: string
}
let rec evaluate (item: string) (target: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable item = item
    let mutable target = target
    try
        let mutable score: int = 0
        let mutable i: int = 0
        while (i < (String.length (item))) && (i < (String.length (target))) do
            if (_substring item i (i + 1)) = (_substring target i (i + 1)) then
                score <- score + 1
            i <- i + 1
        __ret <- score
        raise Return
        __ret
    with
        | Return -> __ret
and crossover (parent1: string) (parent2: string) =
    let mutable __ret : PairString = Unchecked.defaultof<PairString>
    let mutable parent1 = parent1
    let mutable parent2 = parent2
    try
        let cut: int = (String.length (parent1)) / 2
        let child1: string = (_substring parent1 0 cut) + (_substring parent2 cut (String.length (parent2)))
        let child2: string = (_substring parent2 0 cut) + (_substring parent1 cut (String.length (parent1)))
        __ret <- { first = child1; second = child2 }
        raise Return
        __ret
    with
        | Return -> __ret
and mutate (child: string) (genes: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable child = child
    let mutable genes = genes
    try
        if (String.length (child)) = 0 then
            __ret <- child
            raise Return
        let gene: string = _idx genes (0)
        __ret <- (_substring child 0 ((String.length (child)) - 1)) + gene
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (_str (evaluate ("Helxo Worlx") ("Hello World")))
        let pair: PairString = crossover ("123456") ("abcdef")
        printfn "%s" (pair.first)
        printfn "%s" (pair.second)
        let mut: string = mutate ("123456") (unbox<string array> [|"A"; "B"; "C"; "D"; "E"; "F"|])
        printfn "%s" (mut)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
