// Generated 2025-08-11 17:23 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let LETTERS_AND_SPACE: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz \t\n"
let LOWER: string = "abcdefghijklmnopqrstuvwxyz"
let UPPER: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec to_upper (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable res: string = ""
        let mutable i: int = 0
        try
            while i < (String.length (s)) do
                try
                    let c: string = _substring s (i) (i + 1)
                    let mutable j: int = 0
                    let mutable up: string = c
                    try
                        while j < (String.length (LOWER)) do
                            try
                                if c = (_substring LOWER (j) (j + 1)) then
                                    up <- _substring UPPER (j) (j + 1)
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    res <- res + up
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and char_in (chars: string) (c: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable chars = chars
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (String.length (chars)) do
            if (_substring chars (i) (i + 1)) = c then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and remove_non_letters (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (message)) do
            let ch: string = _substring message (i) (i + 1)
            if char_in (LETTERS_AND_SPACE) (ch) then
                res <- res + ch
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and split_spaces (text: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable text = text
    try
        let mutable res: string array = Array.empty<string>
        let mutable current: string = ""
        let mutable i: int = 0
        while i < (String.length (text)) do
            let ch: string = _substring text (i) (i + 1)
            if ch = " " then
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
and load_dictionary () =
    let mutable __ret : System.Collections.Generic.IDictionary<string, bool> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, bool>>
    try
        let words: string array = unbox<string array> [|"HELLO"; "WORLD"; "HOW"; "ARE"; "YOU"; "THE"; "QUICK"; "BROWN"; "FOX"; "JUMPS"; "OVER"; "LAZY"; "DOG"|]
        let mutable dict: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
        for w in Seq.map string (words) do
            dict.[w] <- true
        __ret <- dict
        raise Return
        __ret
    with
        | Return -> __ret
let ENGLISH_WORDS: System.Collections.Generic.IDictionary<string, bool> = load_dictionary()
let rec get_english_count (message: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable message = message
    try
        let upper: string = to_upper (message)
        let cleaned: string = remove_non_letters (upper)
        let possible: string array = split_spaces (cleaned)
        let mutable matches: int = 0
        let mutable total: int = 0
        for w in Seq.map string (possible) do
            if w <> "" then
                total <- total + 1
                if ENGLISH_WORDS.ContainsKey(w) then
                    matches <- matches + 1
        if total = 0 then
            __ret <- 0.0
            raise Return
        __ret <- (float matches) / (float total)
        raise Return
        __ret
    with
        | Return -> __ret
and is_english (message: string) (word_percentage: int) (letter_percentage: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable message = message
    let mutable word_percentage = word_percentage
    let mutable letter_percentage = letter_percentage
    try
        let words_match: bool = ((get_english_count (message)) * 100.0) >= (float word_percentage)
        let num_letters: int = String.length (remove_non_letters (message))
        let letters_pct: float = if (String.length (message)) = 0 then 0.0 else (((float num_letters) / (float (String.length (message)))) * 100.0)
        let letters_match: bool = letters_pct >= (float letter_percentage)
        __ret <- words_match && letters_match
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (is_english ("Hello World") (20) (85)))
printfn "%s" (_str (is_english ("llold HorWd") (20) (85)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
