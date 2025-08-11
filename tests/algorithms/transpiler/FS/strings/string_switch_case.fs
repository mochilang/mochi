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

let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec split_words (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable words: string array = Array.empty<string>
        let mutable current: string = ""
        for ch in Seq.map string (s) do
            if ch = " " then
                if current <> "" then
                    words <- Array.append words [|current|]
                    current <- ""
            else
                current <- current + ch
        if current <> "" then
            words <- Array.append words [|current|]
        __ret <- words
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_alnum (c: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable c = c
    try
        __ret <- ((("0123456789".Contains(c)) || ("abcdefghijklmnopqrstuvwxyz".Contains(c))) || ("ABCDEFGHIJKLMNOPQRSTUVWXYZ".Contains(c))) || (c = " ")
        raise Return
        __ret
    with
        | Return -> __ret
let rec split_input (text: string) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable text = text
    try
        let mutable result: string array array = Array.empty<string array>
        let mutable current: string = ""
        for ch in Seq.map string (text) do
            if is_alnum (ch) then
                current <- current + ch
            else
                if current <> "" then
                    result <- Array.append result [|(split_words (current))|]
                    current <- ""
        if current <> "" then
            result <- Array.append result [|(split_words (current))|]
        __ret <- result
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
        if (String.length (word)) = 1 then
            __ret <- unbox<string> (word.ToUpper())
            raise Return
        __ret <- unbox<string> (((_substring word (0) (1)).ToUpper()) + ((_substring word (1) (String.length (word))).ToLower()))
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_simple_case (text: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    try
        let parts: string array array = split_input (text)
        let mutable res: string = ""
        for sub in parts do
            for w in Seq.map string (sub) do
                res <- res + (capitalize (w))
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_complex_case (text: string) (upper_flag: bool) (sep: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    let mutable upper_flag = upper_flag
    let mutable sep = sep
    try
        let parts: string array array = split_input (text)
        let mutable res: string = ""
        for sub in parts do
            let mutable first: bool = true
            for w in Seq.map string (sub) do
                let mutable word: string = if upper_flag then (w.ToUpper()) else (w.ToLower())
                if first then
                    res <- res + word
                    first <- false
                else
                    res <- (res + sep) + word
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_pascal_case (text: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    try
        __ret <- to_simple_case (text)
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_camel_case (text: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    try
        let s: string = to_simple_case (text)
        if (String.length (s)) = 0 then
            __ret <- ""
            raise Return
        __ret <- (unbox<string> ((_substring s (0) (1)).ToLower())) + (_substring s (1) (String.length (s)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_snake_case (text: string) (upper_flag: bool) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    let mutable upper_flag = upper_flag
    try
        __ret <- to_complex_case (text) (upper_flag) ("_")
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_kebab_case (text: string) (upper_flag: bool) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    let mutable upper_flag = upper_flag
    try
        __ret <- to_complex_case (text) (upper_flag) ("-")
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (to_pascal_case ("one two 31235three4four"))
printfn "%s" (to_camel_case ("one two 31235three4four"))
printfn "%s" (to_snake_case ("one two 31235three4four") (true))
printfn "%s" (to_snake_case ("one two 31235three4four") (false))
printfn "%s" (to_kebab_case ("one two 31235three4four") (true))
printfn "%s" (to_kebab_case ("one two 31235three4four") (false))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
