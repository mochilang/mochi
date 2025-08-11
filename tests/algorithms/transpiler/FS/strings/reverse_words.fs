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
and reverse_words (input_str: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable input_str = input_str
    try
        let mutable words: string array = split_words (input_str)
        let mutable res: string = ""
        let mutable i: int = (Seq.length (words)) - 1
        while i >= 0 do
            res <- res + (_idx words (int i))
            if i > 0 then
                res <- res + " "
            i <- i - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (reverse_words ("I love Python"))
        printfn "%s" (reverse_words ("I     Love          Python"))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
