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

let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = ch then
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
        let idx: int = indexOf (upper) (ch)
        if idx >= 0 then
            __ret <- 65 + idx
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
        if (n >= 65) && (n < 91) then
            __ret <- _substring upper (n - 65) (n - 64)
            raise Return
        __ret <- "?"
        raise Return
        __ret
    with
        | Return -> __ret
let rec vernam_encrypt (plaintext: string) (key: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable plaintext = plaintext
    let mutable key = key
    try
        let mutable ciphertext: string = ""
        let mutable i: int = 0
        while i < (String.length (plaintext)) do
            let p: int = (ord (_substring plaintext i (i + 1))) - 65
            let k: int = (ord (_substring key (((i % (String.length (key)) + (String.length (key))) % (String.length (key)))) ((((i % (String.length (key)) + (String.length (key))) % (String.length (key)))) + 1))) - 65
            let mutable ct: int = p + k
            while ct > 25 do
                ct <- ct - 26
            ciphertext <- ciphertext + (chr (ct + 65))
            i <- i + 1
        __ret <- ciphertext
        raise Return
        __ret
    with
        | Return -> __ret
let rec vernam_decrypt (ciphertext: string) (key: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ciphertext = ciphertext
    let mutable key = key
    try
        let mutable decrypted: string = ""
        let mutable i: int = 0
        while i < (String.length (ciphertext)) do
            let c: int = ord (_substring ciphertext i (i + 1))
            let k: int = ord (_substring key (((i % (String.length (key)) + (String.length (key))) % (String.length (key)))) ((((i % (String.length (key)) + (String.length (key))) % (String.length (key)))) + 1))
            let mutable ``val``: int = c - k
            while ``val`` < 0 do
                ``val`` <- ``val`` + 26
            decrypted <- decrypted + (chr (``val`` + 65))
            i <- i + 1
        __ret <- decrypted
        raise Return
        __ret
    with
        | Return -> __ret
let plaintext: string = "HELLO"
let key: string = "KEY"
let encrypted: string = vernam_encrypt (plaintext) (key)
let mutable decrypted: string = vernam_decrypt (encrypted) (key)
printfn "%s" ("Plaintext: " + plaintext)
printfn "%s" ("Encrypted: " + encrypted)
printfn "%s" ("Decrypted: " + decrypted)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
