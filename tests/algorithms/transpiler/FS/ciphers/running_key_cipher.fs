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
let rec indexOf (s: string) (ch: string) =
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
let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let mutable idx: int = indexOf (upper) (ch)
        if idx >= 0 then
            __ret <- 65 + idx
            raise Return
        idx <- indexOf (lower) (ch)
        if idx >= 0 then
            __ret <- 97 + idx
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec chr (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        if (n >= 65) && (n < 91) then
            __ret <- upper.Substring(n - 65, (n - 64) - (n - 65))
            raise Return
        if (n >= 97) && (n < 123) then
            __ret <- lower.Substring(n - 97, (n - 96) - (n - 97))
            raise Return
        __ret <- "?"
        raise Return
        __ret
    with
        | Return -> __ret
let rec clean_text (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = string (s.[i])
            if (ch >= "A") && (ch <= "Z") then
                out <- out + ch
            else
                if (ch >= "a") && (ch <= "z") then
                    out <- out + (chr ((ord (ch)) - 32))
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec running_key_encrypt (key: string) (plaintext: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable plaintext = plaintext
    try
        let pt: string = clean_text (plaintext)
        let k: string = clean_text (key)
        let key_len: int = String.length (k)
        let mutable res: string = ""
        let ord_a: int = ord ("A")
        let mutable i: int = 0
        while i < (String.length (pt)) do
            let p: int = (ord (string (pt.[i]))) - ord_a
            let kv: int = (ord (string (k.[((i % key_len + key_len) % key_len)]))) - ord_a
            let c: int = (((p + kv) % 26 + 26) % 26)
            res <- res + (chr (c + ord_a))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec running_key_decrypt (key: string) (ciphertext: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable ciphertext = ciphertext
    try
        let ct: string = clean_text (ciphertext)
        let k: string = clean_text (key)
        let key_len: int = String.length (k)
        let mutable res: string = ""
        let ord_a: int = ord ("A")
        let mutable i: int = 0
        while i < (String.length (ct)) do
            let c: int = (ord (string (ct.[i]))) - ord_a
            let kv: int = (ord (string (k.[((i % key_len + key_len) % key_len)]))) - ord_a
            let p: int = ((((c - kv) + 26) % 26 + 26) % 26)
            res <- res + (chr (p + ord_a))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let key: string = "How does the duck know that? said Victor"
let plaintext: string = "DEFEND THIS"
let ciphertext: string = running_key_encrypt (key) (plaintext)
printfn "%s" (ciphertext)
printfn "%s" (running_key_decrypt (key) (ciphertext))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
