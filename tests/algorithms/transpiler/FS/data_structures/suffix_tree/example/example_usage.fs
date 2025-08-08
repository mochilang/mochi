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
type SuffixTree = {
    mutable _text: string
}
let rec new_suffix_tree (_text: string) =
    let mutable __ret : SuffixTree = Unchecked.defaultof<SuffixTree>
    let mutable _text = _text
    try
        __ret <- { _text = _text }
        raise Return
        __ret
    with
        | Return -> __ret
and search (tree: SuffixTree) (pattern: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable tree = tree
    let mutable pattern = pattern
    try
        let n: int = String.length (tree._text)
        let m: int = String.length (pattern)
        if m = 0 then
            __ret <- true
            raise Return
        if m > n then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i <= (n - m) do
            if (_substring (tree._text) (i) (i + m)) = pattern then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let _text: string = "monkey banana"
        let suffix_tree: SuffixTree = new_suffix_tree (_text)
        let patterns: string array = [|"ana"; "ban"; "na"; "xyz"; "mon"|]
        let mutable i: int = 0
        while i < (Seq.length (patterns)) do
            let pattern: string = _idx patterns (i)
            let found: bool = search (suffix_tree) (pattern)
            printfn "%s" ((("Pattern '" + pattern) + "' found: ") + (_str (found)))
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
