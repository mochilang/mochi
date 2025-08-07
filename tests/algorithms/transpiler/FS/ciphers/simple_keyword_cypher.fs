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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec index_in_string (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (string (s.[i])) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains_char (s: string) (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable ch = ch
    try
        __ret <- (index_in_string (s) (ch)) >= 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_alpha (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        __ret <- (contains_char (lower) (ch)) || (contains_char (upper) (ch))
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_upper (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = string (s.[i])
            let idx: int = index_in_string (lower) (ch)
            if idx >= 0 then
                res <- res + (string (upper.[idx]))
            else
                res <- res + ch
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_duplicates (key: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (key)) do
            let ch: string = string (key.[i])
            if (ch = " ") || ((is_alpha (ch)) && ((contains_char (res) (ch)) = false)) then
                res <- res + ch
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec create_cipher_map (key: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable key = key
    try
        let alphabet: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let cleaned: string = remove_duplicates (to_upper (key))
        let mutable cipher: string array = [||]
        let mutable i: int = 0
        while i < (String.length (cleaned)) do
            cipher <- Array.append cipher [|string (cleaned.[i])|]
            i <- i + 1
        let mutable offset: int = String.length (cleaned)
        let mutable j: int = Seq.length (cipher)
        while j < 26 do
            let mutable char: string = string (alphabet.[j - offset])
            while contains_char (cleaned) (char) do
                offset <- offset - 1
                char <- string (alphabet.[j - offset])
            cipher <- Array.append cipher [|char|]
            j <- j + 1
        __ret <- cipher
        raise Return
        __ret
    with
        | Return -> __ret
let rec index_in_list (lst: string array) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable lst = lst
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            if (_idx lst (i)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec encipher (message: string) (cipher: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    let mutable cipher = cipher
    try
        let alphabet: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let msg: string = to_upper (message)
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (msg)) do
            let ch: string = string (msg.[i])
            let idx: int = index_in_string (alphabet) (ch)
            if idx >= 0 then
                res <- res + (_idx cipher (idx))
            else
                res <- res + ch
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec decipher (message: string) (cipher: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    let mutable cipher = cipher
    try
        let alphabet: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let msg: string = to_upper (message)
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (msg)) do
            let ch: string = string (msg.[i])
            let idx: int = index_in_list (cipher) (ch)
            if idx >= 0 then
                res <- res + (string (alphabet.[idx]))
            else
                res <- res + ch
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let cipher_map: string array = create_cipher_map ("Goodbye!!")
let encoded: string = encipher ("Hello World!!") (cipher_map)
printfn "%s" (encoded)
printfn "%s" (decipher (encoded) (cipher_map))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
