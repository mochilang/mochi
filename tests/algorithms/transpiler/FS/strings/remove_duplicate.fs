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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec split_ws (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable res: string array = Array.empty<string>
        let mutable word: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s i (i + 1)
            if ch = " " then
                if word <> "" then
                    res <- Array.append res [|word|]
                    word <- ""
            else
                word <- word + ch
            i <- i + 1
        if word <> "" then
            res <- Array.append res [|word|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains (xs: string array) (x: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (int i)) = x then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec unique (xs: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    try
        let mutable res: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            let w: string = _idx xs (int i)
            if not (contains (res) (w)) then
                res <- Array.append res [|w|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec insertion_sort (arr: string array) =
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
let rec join_with_space (xs: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i > 0 then
                s <- s + " "
            s <- s + (_idx xs (int i))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_duplicates (sentence: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable sentence = sentence
    try
        let words: string array = split_ws (sentence)
        let uniq: string array = unique (words)
        let sorted_words: string array = insertion_sort (uniq)
        __ret <- join_with_space (sorted_words)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (remove_duplicates ("Python is great and Java is also great"))
printfn "%s" (remove_duplicates ("Python   is      great and Java is also great"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
