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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let LETTERS: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let LOWERCASE: string = "abcdefghijklmnopqrstuvwxyz"
let mutable seed: int = 1
let rec rand (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        seed <- ((((seed * 1664525) + 1013904223) % 2147483647 + 2147483647) % 2147483647)
        __ret <- ((seed % n + n) % n)
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_random_key () =
    let mutable __ret : string = Unchecked.defaultof<string>
    try
        let mutable chars: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (String.length (LETTERS)) do
            chars <- Array.append chars [|string (LETTERS.[i])|]
            i <- i + 1
        let mutable j: int = (Seq.length (chars)) - 1
        while j > 0 do
            let k: int = rand (j + 1)
            let tmp: string = _idx chars (j)
            chars.[j] <- _idx chars (k)
            chars.[k] <- tmp
            j <- j - 1
        let mutable res: string = ""
        i <- 0
        while i < (Seq.length (chars)) do
            res <- res + (_idx chars (i))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec check_valid_key (key: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable key = key
    try
        if (String.length (key)) <> (String.length (LETTERS)) then
            __ret <- false
            raise Return
        let mutable used: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
        let mutable i: int = 0
        while i < (String.length (key)) do
            let ch: string = string (key.[i])
            if used.[(string (ch))] then
                __ret <- false
                raise Return
            used.[ch] <- true
            i <- i + 1
        i <- 0
        while i < (String.length (LETTERS)) do
            let ch: string = string (LETTERS.[i])
            if not (used.[(string (ch))]) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec index_in (s: string) (ch: string) =
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
let rec char_to_upper (c: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (String.length (LOWERCASE)) do
            if c = (string (LOWERCASE.[i])) then
                __ret <- string (LETTERS.[i])
                raise Return
            i <- i + 1
        __ret <- c
        raise Return
        __ret
    with
        | Return -> __ret
let rec char_to_lower (c: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (String.length (LETTERS)) do
            if c = (string (LETTERS.[i])) then
                __ret <- string (LOWERCASE.[i])
                raise Return
            i <- i + 1
        __ret <- c
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_upper (c: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (String.length (LETTERS)) do
            if c = (string (LETTERS.[i])) then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec translate_message (key: string) (message: string) (mode: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable message = message
    let mutable mode = mode
    try
        let mutable chars_a: string = LETTERS
        let mutable chars_b: string = key
        if mode = "decrypt" then
            let tmp: string = chars_a
            chars_a <- chars_b
            chars_b <- tmp
        let mutable translated: string = ""
        let mutable i: int = 0
        while i < (String.length (message)) do
            let symbol: string = string (message.[i])
            let upper_symbol: string = char_to_upper (symbol)
            let idx: int = index_in (chars_a) (upper_symbol)
            if idx >= 0 then
                let mapped: string = string (chars_b.[idx])
                if is_upper (symbol) then
                    translated <- translated + mapped
                else
                    translated <- translated + (char_to_lower (mapped))
            else
                translated <- translated + symbol
            i <- i + 1
        __ret <- translated
        raise Return
        __ret
    with
        | Return -> __ret
let rec encrypt_message (key: string) (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable message = message
    try
        let mutable res: string = translate_message (key) (message) ("encrypt")
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec decrypt_message (key: string) (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable message = message
    try
        let mutable res: string = translate_message (key) (message) ("decrypt")
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let key: string = "LFWOAYUISVKMNXPBDCRJTQEGHZ"
printfn "%s" (encrypt_message (key) ("Harshil Darji"))
printfn "%s" (decrypt_message (key) ("Ilcrism Olcvs"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
