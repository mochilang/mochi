// Generated 2025-08-06 23:33 +0700

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
let ALPHABET: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec index_of (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        for i in 0 .. ((String.length (ALPHABET)) - 1) do
            if (string (ALPHABET.[i])) = ch then
                __ret <- i
                raise Return
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and generate_key (message: string) (key: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    let mutable key = key
    try
        let mutable key_new: string = key
        let mutable i: int = 0
        while (String.length (key_new)) < (String.length (message)) do
            key_new <- key_new + (string (key.[i]))
            i <- i + 1
            if i = (String.length (key)) then
                i <- 0
        __ret <- key_new
        raise Return
        __ret
    with
        | Return -> __ret
and cipher_text (message: string) (key_new: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    let mutable key_new = key_new
    try
        let mutable res: string = ""
        let mutable i: int = 0
        for idx in 0 .. ((String.length (message)) - 1) do
            let ch: string = string (message.[idx])
            if ch = " " then
                res <- res + " "
            else
                let x: int = (((((index_of (ch)) - (index_of (string (key_new.[i])))) + 26) % 26 + 26) % 26)
                i <- i + 1
                res <- res + (string (ALPHABET.[x]))
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and original_text (cipher: string) (key_new: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable cipher = cipher
    let mutable key_new = key_new
    try
        let mutable res: string = ""
        let mutable i: int = 0
        for idx in 0 .. ((String.length (cipher)) - 1) do
            let ch: string = string (cipher.[idx])
            if ch = " " then
                res <- res + " "
            else
                let x: int = (((((index_of (ch)) + (index_of (string (key_new.[i])))) + 26) % 26 + 26) % 26)
                i <- i + 1
                res <- res + (string (ALPHABET.[x]))
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let message: string = "THE GERMAN ATTACK"
let key: string = "SECRET"
let mutable key_new: string = generate_key (message) (key)
let encrypted: string = cipher_text (message) (key_new)
printfn "%s" ("Encrypted Text = " + encrypted)
printfn "%s" ("Original Text = " + (original_text (encrypted) (key_new)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
