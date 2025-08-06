// Generated 2025-08-06 23:33 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let encode_map: System.Collections.Generic.IDictionary<string, string> = _dictCreate [("a", "AAAAA"); ("b", "AAAAB"); ("c", "AAABA"); ("d", "AAABB"); ("e", "AABAA"); ("f", "AABAB"); ("g", "AABBA"); ("h", "AABBB"); ("i", "ABAAA"); ("j", "BBBAA"); ("k", "ABAAB"); ("l", "ABABA"); ("m", "ABABB"); ("n", "ABBAA"); ("o", "ABBAB"); ("p", "ABBBA"); ("q", "ABBBB"); ("r", "BAAAA"); ("s", "BAAAB"); ("t", "BAABA"); ("u", "BAABB"); ("v", "BBBAB"); ("w", "BABAA"); ("x", "BABAB"); ("y", "BABBA"); ("z", "BABBB"); (" ", " ")]
let rec make_decode_map () =
    let mutable __ret : System.Collections.Generic.IDictionary<string, string> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, string>>
    try
        let mutable m: System.Collections.Generic.IDictionary<string, string> = _dictCreate []
        for k in encode_map.Keys do
            m.[encode_map.[(string (k))]] <- k
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
let decode_map: System.Collections.Generic.IDictionary<string, string> = make_decode_map()
let rec split_spaces (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable parts: string array = [||]
        let mutable current: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s i (i + 1)
            if ch = " " then
                parts <- Array.append parts [|current|]
                current <- ""
            else
                current <- current + ch
            i <- i + 1
        parts <- Array.append parts [|current|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
and encode (word: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable word = word
    try
        let mutable w: string = word.ToLower()
        let mutable encoded: string = ""
        let mutable i: int = 0
        while i < (String.length (w)) do
            let ch: string = _substring w i (i + 1)
            if encode_map.ContainsKey(ch) then
                encoded <- encoded + (encode_map.[(string (ch))])
            else
                failwith ("encode() accepts only letters of the alphabet and spaces")
            i <- i + 1
        __ret <- encoded
        raise Return
        __ret
    with
        | Return -> __ret
and decode (coded: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable coded = coded
    try
        let mutable i: int = 0
        while i < (String.length (coded)) do
            let ch: string = _substring coded i (i + 1)
            if ((ch <> "A") && (ch <> "B")) && (ch <> " ") then
                failwith ("decode() accepts only 'A', 'B' and spaces")
            i <- i + 1
        let words: string array = split_spaces (coded)
        let mutable decoded: string = ""
        let mutable w: int = 0
        while w < (Seq.length (words)) do
            let word: string = _idx words (w)
            let mutable j: int = 0
            while j < (String.length (word)) do
                let segment: string = _substring word j (j + 5)
                decoded <- decoded + (decode_map.[(string (segment))])
                j <- j + 5
            if w < ((Seq.length (words)) - 1) then
                decoded <- decoded + " "
            w <- w + 1
        __ret <- decoded
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (encode ("hello"))
printfn "%s" (encode ("hello world"))
printfn "%s" (decode ("AABBBAABAAABABAABABAABBAB BABAAABBABBAAAAABABAAAABB"))
printfn "%s" (decode ("AABBBAABAAABABAABABAABBAB"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
