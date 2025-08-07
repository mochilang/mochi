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
let LETTERS: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec find_char (s: string) (ch: string) =
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
and encrypt_message (key: string) (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable message = message
    try
        let chars_a: string = key
        let chars_b: string = LETTERS
        let mutable translated: string = ""
        let mutable i: int = 0
        while i < (String.length (message)) do
            let symbol: string = string (message.[i])
            let upper_sym: string = symbol.ToUpper()
            let sym_index: int = find_char (chars_a) (upper_sym)
            if sym_index >= 0 then
                let sub_char: string = string (chars_b.[sym_index])
                if symbol = upper_sym then
                    translated <- translated + (unbox<string> (sub_char.ToUpper()))
                else
                    translated <- translated + (unbox<string> (sub_char.ToLower()))
            else
                translated <- translated + symbol
            i <- i + 1
        __ret <- translated
        raise Return
        __ret
    with
        | Return -> __ret
and decrypt_message (key: string) (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable message = message
    try
        let chars_a: string = LETTERS
        let chars_b: string = key
        let mutable translated: string = ""
        let mutable i: int = 0
        while i < (String.length (message)) do
            let symbol: string = string (message.[i])
            let upper_sym: string = symbol.ToUpper()
            let sym_index: int = find_char (chars_a) (upper_sym)
            if sym_index >= 0 then
                let sub_char: string = string (chars_b.[sym_index])
                if symbol = upper_sym then
                    translated <- translated + (unbox<string> (sub_char.ToUpper()))
                else
                    translated <- translated + (unbox<string> (sub_char.ToLower()))
            else
                translated <- translated + symbol
            i <- i + 1
        __ret <- translated
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let message: string = "Hello World"
        let key: string = "QWERTYUIOPASDFGHJKLZXCVBNM"
        let mode: string = "decrypt"
        let mutable translated: string = ""
        if mode = "encrypt" then
            translated <- encrypt_message (key) (message)
        else
            if mode = "decrypt" then
                translated <- decrypt_message (key) (message)
        printfn "%s" ((((("Using the key " + key) + ", the ") + mode) + "ed message is: ") + translated)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
