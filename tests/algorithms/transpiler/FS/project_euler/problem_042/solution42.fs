// Generated 2025-08-22 15:25 +0700

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

let _ord (s:string) : int = int s.[0]
let rec _str v =
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.IO

let rec triangular_numbers (limit: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable limit = limit
    try
        let mutable res: int array = Array.empty<int>
        let mutable n: int = 1
        while n <= limit do
            res <- Array.append res [|int (_floordiv64 (int64 ((int64 n) * (int64 (n + 1)))) (int64 (int64 2)))|]
            n <- n + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and parse_words (text: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable text = text
    try
        let mutable words: string array = Array.empty<string>
        let mutable current: string = ""
        let mutable i: int = 0
        while i < (String.length (text)) do
            let c: string = _substring text (i) (i + 1)
            if c = "," then
                words <- Array.append words [|current|]
                current <- ""
            else
                if c = "\"" then ()
                else
                    if (c = "\r") || (c = "\n") then ()
                    else
                        current <- current + c
            i <- i + 1
        if (String.length (current)) > 0 then
            words <- Array.append words [|current|]
        __ret <- words
        raise Return
        __ret
    with
        | Return -> __ret
and word_value (word: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable word = word
    try
        let mutable total: int = 0
        let mutable i: int = 0
        while i < (String.length (word)) do
            total <- int ((int (total + (int (_ord (_substring word (i) (i + 1)))))) - 64)
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and contains (xs: int array) (target: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable target = target
    try
        for x in xs do
            if x = target then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and solution () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        let text: string = System.IO.File.ReadAllText("words.txt")
        let mutable words: string array = parse_words (text)
        let tri: int array = triangular_numbers (100)
        let mutable count: int = 0
        for w in Seq.map string (words) do
            let v: int = word_value (w)
            if contains (tri) (v) then
                count <- count + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (solution())))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
