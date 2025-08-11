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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

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
open System.Collections.Generic

open System.IO

let rec split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable res: string array = Array.empty<string>
        let mutable current: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s (i) (i + 1)
            if ch = sep then
                res <- Array.append res [|current|]
                current <- ""
            else
                current <- current + ch
            i <- i + 1
        res <- Array.append res [|current|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and insertion_sort (arr: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable arr = arr
    try
        let mutable a: string array = arr
        let mutable i: int = 1
        while i < (Seq.length (a)) do
            let key: string = _idx a (int i)
            let mutable j: int = i - 1
            while (j >= 0) && ((_idx a (int j)) > key) do
                a.[int (j + 1)] <- _idx a (int j)
                j <- j - 1
            a.[int (j + 1)] <- key
            i <- i + 1
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
and sort_chars (word: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable word = word
    try
        let mutable chars: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (String.length (word)) do
            chars <- Array.append chars [|(_substring word (i) (i + 1))|]
            i <- i + 1
        chars <- insertion_sort (chars)
        let mutable res: string = ""
        i <- 0
        while i < (Seq.length (chars)) do
            res <- res + (_idx chars (int i))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and unique_sorted (words: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable words = words
    try
        let mutable seen: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
        let mutable res: string array = Array.empty<string>
        for w in Seq.map string (words) do
            if (w <> "") && (unbox<bool> (not (seen.ContainsKey(w)))) then
                res <- Array.append res [|w|]
                seen.[w] <- true
        res <- insertion_sort (res)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let mutable word_by_signature: System.Collections.Generic.IDictionary<string, string array> = _dictCreate []
let rec build_map (words: string array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable words = words
    try
        for w in Seq.map string (words) do
            let ``sig``: string = sort_chars (w)
            let mutable arr: string array = Array.empty<string>
            if word_by_signature.ContainsKey(``sig``) then
                arr <- _dictGet word_by_signature ((string (``sig``)))
            arr <- Array.append arr [|w|]
            word_by_signature.[``sig``] <- arr
        __ret
    with
        | Return -> __ret
and anagram (my_word: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable my_word = my_word
    try
        let ``sig``: string = sort_chars (my_word)
        if word_by_signature.ContainsKey(``sig``) then
            __ret <- _dictGet word_by_signature ((string (``sig``)))
            raise Return
        __ret <- Array.empty<string>
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let text: string = System.IO.File.ReadAllText("words.txt")
        let lines: string array = split (text) ("\n")
        let words: string array = unique_sorted (lines)
        build_map (words)
        for w in Seq.map string (words) do
            let anas: string array = anagram (w)
            if (Seq.length (anas)) > 1 then
                let mutable line: string = w + ":"
                let mutable i: int = 0
                while i < (Seq.length (anas)) do
                    if i > 0 then
                        line <- line + ","
                    line <- line + (_idx anas (int i))
                    i <- i + 1
                printfn "%s" (line)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
