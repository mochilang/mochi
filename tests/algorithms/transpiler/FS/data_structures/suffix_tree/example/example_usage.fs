// Generated 2025-08-07 14:57 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type SuffixTree = {
    text: string
}
let rec new_suffix_tree (text: string) =
    let mutable __ret : SuffixTree = Unchecked.defaultof<SuffixTree>
    let mutable text = text
    try
        __ret <- { text = text }
        raise Return
        __ret
    with
        | Return -> __ret
and search (tree: SuffixTree) (pattern: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable tree = tree
    let mutable pattern = pattern
    try
        let n: int = String.length (tree.text)
        let m: int = String.length (pattern)
        if m = 0 then
            __ret <- true
            raise Return
        if m > n then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i <= (n - m) do
            if (tree.text.Substring(i, (i + m) - i)) = pattern then
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
        let text: string = "monkey banana"
        let suffix_tree: SuffixTree = new_suffix_tree (text)
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
