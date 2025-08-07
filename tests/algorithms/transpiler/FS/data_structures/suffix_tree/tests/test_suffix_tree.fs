// Generated 2025-08-07 14:57 +0700

exception Break
exception Continue

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec suffix_tree_new (text: string) =
    let mutable __ret : SuffixTree = Unchecked.defaultof<SuffixTree>
    let mutable text = text
    try
        __ret <- { text = text }
        raise Return
        __ret
    with
        | Return -> __ret
let rec suffix_tree_search (st: SuffixTree) (pattern: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable st = st
    let mutable pattern = pattern
    try
        if (String.length (pattern)) = 0 then
            __ret <- true
            raise Return
        let mutable i: int = 0
        let n: int = String.length (st.text)
        let m: int = String.length (pattern)
        try
            while i <= (n - m) do
                try
                    let mutable j: int = 0
                    let mutable found: bool = true
                    try
                        while j < m do
                            try
                                if (string (st.text.[i + j])) <> (string (pattern.[j])) then
                                    found <- false
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if found then
                        __ret <- true
                        raise Return
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let text: string = "banana"
let st: SuffixTree = suffix_tree_new (text)
let patterns_exist: string array = [|"ana"; "ban"; "na"|]
let mutable i: int = 0
while i < (Seq.length (patterns_exist)) do
    printfn "%s" (_str (suffix_tree_search (st) (_idx patterns_exist (i))))
    i <- i + 1
let patterns_none: string array = [|"xyz"; "apple"; "cat"|]
i <- 0
while i < (Seq.length (patterns_none)) do
    printfn "%s" (_str (suffix_tree_search (st) (_idx patterns_none (i))))
    i <- i + 1
printfn "%s" (_str (suffix_tree_search (st) ("")))
printfn "%s" (_str (suffix_tree_search (st) (text)))
let substrings: string array = [|"ban"; "ana"; "a"; "na"|]
i <- 0
while i < (Seq.length (substrings)) do
    printfn "%s" (_str (suffix_tree_search (st) (_idx substrings (i))))
    i <- i + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
