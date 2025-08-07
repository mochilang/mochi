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
let CHARS: string array = [|"A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "M"; "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "0"; "&"; "@"; ":"; ","; "."; "'"; "\""; "?"; "/"; "="; "+"; "-"; "("; ")"; "!"; " "|]
let CODES: string array = [|".-"; "-..."; "-.-."; "-.."; "."; "..-."; "--."; "...."; ".."; ".---"; "-.-"; ".-.."; "--"; "-."; "---"; ".--."; "--.-"; ".-."; "..."; "-"; "..-"; "...-"; ".--"; "-..-"; "-.--"; "--.."; ".----"; "..---"; "...--"; "....-"; "....."; "-...."; "--..."; "---.."; "----."; "-----"; ".-..."; ".--.-."; "---..."; "--..--"; ".-.-.-"; ".----."; ".-..-."; "..--.."; "-..-."; "-...-"; ".-.-."; "-....-"; "-.--."; "-.--.-"; "-.-.--"; "/"|]
let rec to_upper_char (c: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    try
        if c = "a" then
            __ret <- "A"
            raise Return
        if c = "b" then
            __ret <- "B"
            raise Return
        if c = "c" then
            __ret <- "C"
            raise Return
        if c = "d" then
            __ret <- "D"
            raise Return
        if c = "e" then
            __ret <- "E"
            raise Return
        if c = "f" then
            __ret <- "F"
            raise Return
        if c = "g" then
            __ret <- "G"
            raise Return
        if c = "h" then
            __ret <- "H"
            raise Return
        if c = "i" then
            __ret <- "I"
            raise Return
        if c = "j" then
            __ret <- "J"
            raise Return
        if c = "k" then
            __ret <- "K"
            raise Return
        if c = "l" then
            __ret <- "L"
            raise Return
        if c = "m" then
            __ret <- "M"
            raise Return
        if c = "n" then
            __ret <- "N"
            raise Return
        if c = "o" then
            __ret <- "O"
            raise Return
        if c = "p" then
            __ret <- "P"
            raise Return
        if c = "q" then
            __ret <- "Q"
            raise Return
        if c = "r" then
            __ret <- "R"
            raise Return
        if c = "s" then
            __ret <- "S"
            raise Return
        if c = "t" then
            __ret <- "T"
            raise Return
        if c = "u" then
            __ret <- "U"
            raise Return
        if c = "v" then
            __ret <- "V"
            raise Return
        if c = "w" then
            __ret <- "W"
            raise Return
        if c = "x" then
            __ret <- "X"
            raise Return
        if c = "y" then
            __ret <- "Y"
            raise Return
        if c = "z" then
            __ret <- "Z"
            raise Return
        __ret <- c
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_upper (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            res <- res + (to_upper_char (string (s.[i])))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec index_of (xs: string array) (target: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    let mutable target = target
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) = target then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec encrypt (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    try
        let msg: string = to_upper (message)
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (msg)) do
            let c: string = string (msg.[i])
            let idx: int = index_of (CHARS) (c)
            if idx >= 0 then
                if res <> "" then
                    res <- res + " "
                res <- res + (_idx CODES (idx))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec split_spaces (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable res: string array = [||]
        let mutable current: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = string (s.[i])
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
let rec decrypt (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    try
        let parts: string array = split_spaces (message)
        let mutable res: string = ""
        for code in Seq.map string (parts) do
            let idx: int = index_of (CODES) (code)
            if idx >= 0 then
                res <- res + (_idx CHARS (idx))
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let msg: string = "Morse code here!"
printfn "%s" (msg)
let enc: string = encrypt (msg)
printfn "%s" (enc)
let dec: string = decrypt (enc)
printfn "%s" (dec)
printfn "%s" (encrypt ("Sos!"))
printfn "%s" (decrypt ("... --- ... -.-.--"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
