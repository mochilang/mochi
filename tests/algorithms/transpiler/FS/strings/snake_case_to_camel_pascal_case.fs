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
let rec split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable res: string array = Array.empty<string>
        let mutable current: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s i (i + 1)
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
let rec capitalize (word: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable word = word
    try
        if (String.length (word)) = 0 then
            __ret <- ""
            raise Return
        let first: string = (_substring word 0 1).ToUpper()
        let rest: string = _substring word 1 (String.length (word))
        __ret <- first + rest
        raise Return
        __ret
    with
        | Return -> __ret
let rec snake_to_camel_case (input_str: string) (use_pascal: bool) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable input_str = input_str
    let mutable use_pascal = use_pascal
    try
        let words: string array = split (input_str) ("_")
        let mutable result: string = ""
        let mutable index: int = 0
        if not use_pascal then
            if (Seq.length (words)) > 0 then
                result <- _idx words (int 0)
                index <- 1
        while index < (Seq.length (words)) do
            let word: string = _idx words (int index)
            result <- result + (capitalize (word))
            index <- index + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (snake_to_camel_case ("some_random_string") (false))
printfn "%s" (snake_to_camel_case ("some_random_string") (true))
printfn "%s" (snake_to_camel_case ("some_random_string_with_numbers_123") (false))
printfn "%s" (snake_to_camel_case ("some_random_string_with_numbers_123") (true))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
