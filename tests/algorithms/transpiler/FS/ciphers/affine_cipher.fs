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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let SYMBOLS: string = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
let rec gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        while y <> 0 do
            let temp: int = ((x % y + y) % y)
            x <- y
            y <- temp
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and mod_inverse (a: int) (m: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable m = m
    try
        if (gcd (a) (m)) <> 1 then
            failwith (((("mod inverse of " + (_str (a))) + " and ") + (_str (m))) + " does not exist")
        let mutable u1: int = 1
        let mutable u2: int = 0
        let mutable u3: int = a
        let mutable v1: int = 0
        let mutable v2: int = 1
        let mutable v3: int = m
        while v3 <> 0 do
            let q: int = u3 / v3
            let t1: int = u1 - (q * v1)
            let t2: int = u2 - (q * v2)
            let t3: int = u3 - (q * v3)
            u1 <- v1
            u2 <- v2
            u3 <- v3
            v1 <- t1
            v2 <- t2
            v3 <- t3
        let res: int = ((u1 % m + m) % m)
        if res < 0 then
            __ret <- res + m
            raise Return
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and find_symbol (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (SYMBOLS)) do
            if (string (SYMBOLS.[i])) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and check_keys (key_a: int) (key_b: int) (mode: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable key_a = key_a
    let mutable key_b = key_b
    let mutable mode = mode
    try
        let m: int = String.length (SYMBOLS)
        if mode = "encrypt" then
            if key_a = 1 then
                failwith ("The affine cipher becomes weak when key A is set to 1. Choose different key")
            if key_b = 0 then
                failwith ("The affine cipher becomes weak when key B is set to 0. Choose different key")
        if ((key_a < 0) || (key_b < 0)) || (key_b > (m - 1)) then
            failwith ("Key A must be greater than 0 and key B must be between 0 and " + (_str (m - 1)))
        if (gcd (key_a) (m)) <> 1 then
            failwith (((("Key A " + (_str (key_a))) + " and the symbol set size ") + (_str (m))) + " are not relatively prime. Choose a different key.")
        __ret
    with
        | Return -> __ret
and encrypt_message (key: int) (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable message = message
    try
        let m: int = String.length (SYMBOLS)
        let key_a: int = key / m
        let key_b: int = ((key % m + m) % m)
        check_keys (key_a) (key_b) ("encrypt")
        let mutable cipher_text: string = ""
        let mutable i: int = 0
        while i < (String.length (message)) do
            let ch: string = string (message.[i])
            let index: int = find_symbol (ch)
            if index >= 0 then
                cipher_text <- cipher_text + (string (SYMBOLS.[((((index * key_a) + key_b) % m + m) % m)]))
            else
                cipher_text <- cipher_text + ch
            i <- i + 1
        __ret <- cipher_text
        raise Return
        __ret
    with
        | Return -> __ret
and decrypt_message (key: int) (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable message = message
    try
        let m: int = String.length (SYMBOLS)
        let key_a: int = key / m
        let key_b: int = ((key % m + m) % m)
        check_keys (key_a) (key_b) ("decrypt")
        let inv: int = mod_inverse (key_a) (m)
        let mutable plain_text: string = ""
        let mutable i: int = 0
        while i < (String.length (message)) do
            let ch: string = string (message.[i])
            let index: int = find_symbol (ch)
            if index >= 0 then
                let mutable n: int = (index - key_b) * inv
                let pos: int = ((n % m + m) % m)
                let final: int = if pos < 0 then (pos + m) else pos
                plain_text <- plain_text + (string (SYMBOLS.[final]))
            else
                plain_text <- plain_text + ch
            i <- i + 1
        __ret <- plain_text
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let key: int = 4545
        let msg: string = "The affine cipher is a type of monoalphabetic substitution cipher."
        let enc: string = encrypt_message (key) (msg)
        printfn "%s" (enc)
        printfn "%s" (decrypt_message (key) (enc))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
