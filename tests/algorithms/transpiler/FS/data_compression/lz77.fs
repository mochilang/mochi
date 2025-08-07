// Generated 2025-08-07 10:31 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Token = {
    offset: int
    length: int
    indicator: string
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec token_to_string (t: Token) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable t = t
    try
        __ret <- ((((("(" + (_str (t.offset))) + ", ") + (_str (t.length))) + ", ") + (t.indicator)) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
let rec tokens_to_string (ts: Token array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ts = ts
    try
        let mutable res: string = "["
        let mutable i: int = 0
        while i < (Seq.length (ts)) do
            res <- res + (token_to_string (_idx ts (i)))
            if i < ((Seq.length (ts)) - 1) then
                res <- res + ", "
            i <- i + 1
        __ret <- res + "]"
        raise Return
        __ret
    with
        | Return -> __ret
let rec match_length_from_index (text: string) (window: string) (text_index: int) (window_index: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable text = text
    let mutable window = window
    let mutable text_index = text_index
    let mutable window_index = window_index
    try
        if (text_index >= (String.length (text))) || (window_index >= (String.length (window))) then
            __ret <- 0
            raise Return
        let tc: string = _substring text text_index (text_index + 1)
        let wc: string = _substring window window_index (window_index + 1)
        if tc <> wc then
            __ret <- 0
            raise Return
        __ret <- 1 + (match_length_from_index (text) (window + tc) (text_index + 1) (window_index + 1))
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_encoding_token (text: string) (search_buffer: string) =
    let mutable __ret : Token = Unchecked.defaultof<Token>
    let mutable text = text
    let mutable search_buffer = search_buffer
    try
        if (String.length (text)) = 0 then
            failwith ("We need some text to work with.")
        let mutable length: int = 0
        let mutable offset: int = 0
        if (String.length (search_buffer)) = 0 then
            __ret <- { offset = offset; length = length; indicator = _substring text 0 1 }
            raise Return
        let mutable i: int = 0
        while i < (String.length (search_buffer)) do
            let ch: string = _substring search_buffer i (i + 1)
            let found_offset: int = (String.length (search_buffer)) - i
            if ch = (_substring text 0 1) then
                let found_length: int = match_length_from_index (text) (search_buffer) (0) (i)
                if found_length >= length then
                    offset <- found_offset
                    length <- found_length
            i <- i + 1
        __ret <- { offset = offset; length = length; indicator = _substring text length (length + 1) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec lz77_compress (text: string) (window_size: int) (lookahead: int) =
    let mutable __ret : Token array = Unchecked.defaultof<Token array>
    let mutable text = text
    let mutable window_size = window_size
    let mutable lookahead = lookahead
    try
        let search_buffer_size: int = window_size - lookahead
        let mutable output: Token array = [||]
        let mutable search_buffer: string = ""
        let mutable remaining: string = text
        while (String.length (remaining)) > 0 do
            let token: Token = find_encoding_token (remaining) (search_buffer)
            let add_len: int = (token.length) + 1
            search_buffer <- search_buffer + (_substring remaining 0 add_len)
            if (String.length (search_buffer)) > search_buffer_size then
                search_buffer <- _substring search_buffer ((String.length (search_buffer)) - search_buffer_size) (String.length (search_buffer))
            remaining <- _substring remaining add_len (String.length (remaining))
            output <- Array.append output [|token|]
        __ret <- output
        raise Return
        __ret
    with
        | Return -> __ret
let rec lz77_decompress (tokens: Token array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable tokens = tokens
    try
        let mutable output: string = ""
        for t in tokens do
            let mutable i: int = 0
            while i < (t.length) do
                output <- output + (_substring output ((String.length (output)) - (t.offset)) (((String.length (output)) - (t.offset)) + 1))
                i <- i + 1
            output <- output + (t.indicator)
        __ret <- output
        raise Return
        __ret
    with
        | Return -> __ret
let c1: Token array = lz77_compress ("ababcbababaa") (13) (6)
printfn "%s" (tokens_to_string (c1))
let c2: Token array = lz77_compress ("aacaacabcabaaac") (13) (6)
printfn "%s" (tokens_to_string (c2))
let tokens_example: Token array = [|{ offset = 0; length = 0; indicator = "c" }; { offset = 0; length = 0; indicator = "a" }; { offset = 0; length = 0; indicator = "b" }; { offset = 0; length = 0; indicator = "r" }; { offset = 3; length = 1; indicator = "c" }; { offset = 2; length = 1; indicator = "d" }; { offset = 7; length = 4; indicator = "r" }; { offset = 3; length = 5; indicator = "d" }|]
printfn "%s" (lz77_decompress (tokens_example))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
