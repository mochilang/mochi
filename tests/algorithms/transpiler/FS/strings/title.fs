// Generated 2025-08-11 15:32 +0700

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

let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let lower: string = "abcdefghijklmnopqrstuvwxyz"
let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec index_of (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (string (s.[i])) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_title_case (word: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable word = word
    try
        if (String.length (word)) = 0 then
            __ret <- ""
            raise Return
        let first: string = _substring word (0) (1)
        let idx: int = index_of (lower) (first)
        let mutable result: string = if idx >= 0 then (_substring upper (idx) (idx + 1)) else first
        let mutable i: int = 1
        while i < (String.length (word)) do
            let ch: string = _substring word (i) (i + 1)
            let uidx: int = index_of (upper) (ch)
            if uidx >= 0 then
                result <- result + (_substring lower (uidx) (uidx + 1))
            else
                result <- result + ch
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec split_words (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable words: string array = Array.empty<string>
        let mutable current: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = string (s.[i])
            if ch = " " then
                if (String.length (current)) > 0 then
                    words <- Array.append words [|current|]
                    current <- ""
            else
                current <- current + ch
            i <- i + 1
        if (String.length (current)) > 0 then
            words <- Array.append words [|current|]
        __ret <- words
        raise Return
        __ret
    with
        | Return -> __ret
let rec sentence_to_title_case (sentence: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable sentence = sentence
    try
        let mutable words: string array = split_words (sentence)
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length (words)) do
            res <- res + (to_title_case (_idx words (int i)))
            if (i + 1) < (Seq.length (words)) then
                res <- res + " "
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (to_title_case ("Aakash"))
printfn "%s" (to_title_case ("aakash"))
printfn "%s" (to_title_case ("AAKASH"))
printfn "%s" (to_title_case ("aAkAsH"))
printfn "%s" (sentence_to_title_case ("Aakash Giri"))
printfn "%s" (sentence_to_title_case ("aakash giri"))
printfn "%s" (sentence_to_title_case ("AAKASH GIRI"))
printfn "%s" (sentence_to_title_case ("aAkAsH gIrI"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
