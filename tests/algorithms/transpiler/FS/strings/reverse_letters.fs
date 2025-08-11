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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable res: string array = Array.empty<string>
        let mutable current: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = string (s.[i])
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
and join_with_space (xs: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            s <- s + (_idx xs (int i))
            if (i + 1) < (Seq.length (xs)) then
                s <- s + " "
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and reverse_str (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable res: string = ""
        let mutable i: int = (String.length (s)) - 1
        while i >= 0 do
            res <- res + (string (s.[i]))
            i <- i - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and reverse_letters (sentence: string) (length: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable sentence = sentence
    let mutable length = length
    try
        let words: string array = split (sentence) (" ")
        let mutable result: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (Seq.length (words)) do
            let word: string = _idx words (int i)
            if (String.length (word)) > length then
                result <- Array.append result [|(reverse_str (word))|]
            else
                result <- Array.append result [|word|]
            i <- i + 1
        __ret <- join_with_space (result)
        raise Return
        __ret
    with
        | Return -> __ret
and test_reverse_letters () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (reverse_letters ("Hey wollef sroirraw") (3)) <> "Hey fellow warriors" then
            failwith ("test1 failed")
        if (reverse_letters ("nohtyP is nohtyP") (2)) <> "Python is Python" then
            failwith ("test2 failed")
        if (reverse_letters ("1 12 123 1234 54321 654321") (0)) <> "1 21 321 4321 12345 123456" then
            failwith ("test3 failed")
        if (reverse_letters ("racecar") (0)) <> "racecar" then
            failwith ("test4 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_reverse_letters()
        printfn "%s" (reverse_letters ("Hey wollef sroirraw") (3))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
