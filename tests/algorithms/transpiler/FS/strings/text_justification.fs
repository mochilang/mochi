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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec repeat_str (s: string) (count: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable count = count
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < count do
            res <- res + s
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec split_words (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable res: string array = Array.empty<string>
        let mutable current: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s (i) (i + 1)
            if ch = " " then
                if current <> "" then
                    res <- Array.append res [|current|]
                    current <- ""
            else
                current <- current + ch
            i <- i + 1
        if current <> "" then
            res <- Array.append res [|current|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec justify_line (line: string array) (width: int) (max_width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable line = line
    let mutable width = width
    let mutable max_width = max_width
    try
        let overall_spaces_count: int = max_width - width
        let words_count: int = Seq.length (line)
        if words_count = 1 then
            __ret <- (_idx line (int 0)) + (repeat_str (" ") (overall_spaces_count))
            raise Return
        let spaces_to_insert_between_words: int = words_count - 1
        let mutable num_spaces_between_words_list: int array = Array.empty<int>
        let ``base``: int = _floordiv overall_spaces_count spaces_to_insert_between_words
        let extra: int = ((overall_spaces_count % spaces_to_insert_between_words + spaces_to_insert_between_words) % spaces_to_insert_between_words)
        let mutable i: int = 0
        while i < spaces_to_insert_between_words do
            let mutable spaces: int = ``base``
            if i < extra then
                spaces <- spaces + 1
            num_spaces_between_words_list <- Array.append num_spaces_between_words_list [|spaces|]
            i <- i + 1
        let mutable aligned: string = ""
        i <- 0
        while i < spaces_to_insert_between_words do
            aligned <- (aligned + (_idx line (int i))) + (repeat_str (" ") (_idx num_spaces_between_words_list (int i)))
            i <- i + 1
        aligned <- aligned + (_idx line (int spaces_to_insert_between_words))
        __ret <- aligned
        raise Return
        __ret
    with
        | Return -> __ret
let rec text_justification (word: string) (max_width: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable word = word
    let mutable max_width = max_width
    try
        let words: string array = split_words (word)
        let mutable answer: string array = Array.empty<string>
        let mutable line: string array = Array.empty<string>
        let mutable width: int = 0
        let mutable idx: int = 0
        while idx < (Seq.length (words)) do
            let w: string = _idx words (int idx)
            if ((width + (String.length (w))) + (Seq.length (line))) <= max_width then
                line <- Array.append line [|w|]
                width <- width + (String.length (w))
            else
                answer <- Array.append answer [|(justify_line (line) (width) (max_width))|]
                line <- unbox<string array> [|w|]
                width <- String.length (w)
            idx <- idx + 1
        let remaining_spaces: int = (max_width - width) - (Seq.length (line))
        let mutable last_line: string = ""
        let mutable j: int = 0
        while j < (Seq.length (line)) do
            if j > 0 then
                last_line <- last_line + " "
            last_line <- last_line + (_idx line (int j))
            j <- j + 1
        last_line <- last_line + (repeat_str (" ") (remaining_spaces + 1))
        answer <- Array.append answer [|last_line|]
        __ret <- answer
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (text_justification ("This is an example of text justification.") (16)))
printfn "%s" (_str (text_justification ("Two roads diverged in a yellow wood") (16)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
