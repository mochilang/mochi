// Generated 2025-08-22 23:09 +0700

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

let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
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
let rec xor (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: int = 0
        let mutable bit: int = 1
        let mutable x: int = a
        let mutable y: int = b
        while (x > 0) || (y > 0) do
            let abit: int = ((x % 2 + 2) % 2)
            let bbit: int = ((y % 2 + 2) % 2)
            if abit <> bbit then
                res <- res + bit
            x <- _floordiv (int x) (int 2)
            y <- _floordiv (int y) (int 2)
            bit <- int ((int64 bit) * (int64 2))
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let ascii_chars: string = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
let rec chr (code: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable code = code
    try
        if code = 10 then
            __ret <- "\n"
            raise Return
        if code = 13 then
            __ret <- "\r"
            raise Return
        if code = 9 then
            __ret <- "\t"
            raise Return
        if (code >= 32) && (code < 127) then
            __ret <- _substring ascii_chars (code - 32) (code - 31)
            raise Return
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
and ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        if ch = "\n" then
            __ret <- 10
            raise Return
        if ch = "\r" then
            __ret <- 13
            raise Return
        if ch = "\t" then
            __ret <- 9
            raise Return
        let mutable i: int = 0
        while i < (String.length (ascii_chars)) do
            if (_substring ascii_chars (i) (i + 1)) = ch then
                __ret <- 32 + i
                raise Return
            i <- i + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and is_valid_ascii (code: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable code = code
    try
        if (code >= 32) && (code <= 126) then
            __ret <- true
            raise Return
        if ((code = 9) || (code = 10)) || (code = 13) then
            __ret <- true
            raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let mutable LOWERCASE_INTS: int array = Array.empty<int>
let mutable i: int = 97
while i <= 122 do
    LOWERCASE_INTS <- Array.append LOWERCASE_INTS [|i|]
    i <- i + 1
let mutable COMMON_WORDS: string array = unbox<string array> [|"the"; "be"; "to"; "of"; "and"; "in"; "that"; "have"|]
let rec try_key (ciphertext: int array) (key: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ciphertext = ciphertext
    let mutable key = key
    try
        let mutable decoded: string = ""
        let mutable i: int = 0
        let klen: int = Seq.length (key)
        while i < (Seq.length (ciphertext)) do
            let decodedchar: int = xor (_idx ciphertext (int i)) (_idx key (int (((i % klen + klen) % klen))))
            if not (is_valid_ascii (decodedchar)) then
                __ret <- unbox<string> null
                raise Return
            decoded <- decoded + (chr (decodedchar))
            i <- i + 1
        __ret <- decoded
        raise Return
        __ret
    with
        | Return -> __ret
and filter_valid_chars (ciphertext: int array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable ciphertext = ciphertext
    try
        let mutable possibles: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (Seq.length (LOWERCASE_INTS)) do
            let mutable j: int = 0
            while j < (Seq.length (LOWERCASE_INTS)) do
                let mutable k: int = 0
                while k < (Seq.length (LOWERCASE_INTS)) do
                    let key: int array = unbox<int array> [|_idx LOWERCASE_INTS (int i); _idx LOWERCASE_INTS (int j); _idx LOWERCASE_INTS (int k)|]
                    let mutable decoded: string = try_key (ciphertext) (key)
                    if decoded <> (unbox<string> null) then
                        possibles <- Array.append possibles [|decoded|]
                    k <- k + 1
                j <- j + 1
            i <- i + 1
        __ret <- possibles
        raise Return
        __ret
    with
        | Return -> __ret
and contains (s: string) (sub: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable sub = sub
    try
        let n: int = String.length (s)
        let m: int = String.length (sub)
        if m = 0 then
            __ret <- true
            raise Return
        let mutable i: int = 0
        try
            while i <= (n - m) do
                try
                    let mutable j: int = 0
                    let mutable is_match: bool = true
                    try
                        while j < m do
                            try
                                if (string (s.[i + j])) <> (string (sub.[j])) then
                                    is_match <- false
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if is_match then
                        __ret <- true
                        raise Return
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and filter_common_word (possibles: string array) (common_word: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable possibles = possibles
    let mutable common_word = common_word
    try
        let mutable res: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (Seq.length (possibles)) do
            let p: string = _idx possibles (int i)
            if contains (unbox<string> (p.ToLower())) (common_word) then
                res <- Array.append res [|p|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and solution (ciphertext: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ciphertext = ciphertext
    try
        let mutable possibles: string array = filter_valid_chars (ciphertext)
        let mutable i: int = 0
        try
            while i < (Seq.length (COMMON_WORDS)) do
                try
                    let word: string = _idx COMMON_WORDS (int i)
                    possibles <- filter_common_word (possibles) (word)
                    if (Seq.length (possibles)) = 1 then
                        raise Break
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let decoded_text: string = _idx possibles (int 0)
        let mutable sum: int = 0
        let mutable j: int = 0
        while j < (String.length (decoded_text)) do
            sum <- sum + (ord (_substring decoded_text j (j + 1)))
            j <- j + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let ciphertext: int array = unbox<int array> [|17; 6; 1; 69; 12; 1; 69; 26; 11; 69; 1; 2; 69; 15; 10; 1; 78; 13; 11; 78; 16; 13; 15; 16; 69; 6; 5; 19; 11|]
ignore (printfn "%s" (_str (solution (ciphertext))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
