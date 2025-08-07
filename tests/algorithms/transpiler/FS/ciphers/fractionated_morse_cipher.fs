// Generated 2025-08-07 08:16 +0700

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

let MORSE_CODE_DICT: System.Collections.Generic.IDictionary<string, string> = _dictCreate [("A", ".-"); ("B", "-..."); ("C", "-.-."); ("D", "-.."); ("E", "."); ("F", "..-."); ("G", "--."); ("H", "...."); ("I", ".."); ("J", ".---"); ("K", "-.-"); ("L", ".-.."); ("M", "--"); ("N", "-."); ("O", "---"); ("P", ".--."); ("Q", "--.-"); ("R", ".-."); ("S", "..."); ("T", "-"); ("U", "..-"); ("V", "...-"); ("W", ".--"); ("X", "-..-"); ("Y", "-.--"); ("Z", "--.."); (" ", "")]
let MORSE_COMBINATIONS: string array = [|"..."; "..-"; "..x"; ".-."; ".--"; ".-x"; ".x."; ".x-"; ".xx"; "-.."; "-.-"; "-.x"; "--."; "---"; "--x"; "-x."; "-x-"; "-xx"; "x.."; "x.-"; "x.x"; "x-."; "x--"; "x-x"; "xx."; "xx-"; "xxx"|]
let REVERSE_DICT: System.Collections.Generic.IDictionary<string, string> = _dictCreate [(".-", "A"); ("-...", "B"); ("-.-.", "C"); ("-..", "D"); (".", "E"); ("..-.", "F"); ("--.", "G"); ("....", "H"); ("..", "I"); (".---", "J"); ("-.-", "K"); (".-..", "L"); ("--", "M"); ("-.", "N"); ("---", "O"); (".--.", "P"); ("--.-", "Q"); (".-.", "R"); ("...", "S"); ("-", "T"); ("..-", "U"); ("...-", "V"); (".--", "W"); ("-..-", "X"); ("-.--", "Y"); ("--..", "Z"); ("", " ")]
let rec encodeToMorse (plaintext: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable plaintext = plaintext
    try
        let mutable morse: string = ""
        let mutable i: int = 0
        while i < (String.length (plaintext)) do
            let ch: string = (plaintext.Substring(i, (i + 1) - i)).ToUpper()
            let mutable code: string = ""
            if MORSE_CODE_DICT.ContainsKey(ch) then
                code <- MORSE_CODE_DICT.[(string (ch))]
            if i > 0 then
                morse <- morse + "x"
            morse <- morse + code
            i <- i + 1
        __ret <- morse
        raise Return
        __ret
    with
        | Return -> __ret
let rec encryptFractionatedMorse (plaintext: string) (key: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable plaintext = plaintext
    let mutable key = key
    try
        let mutable morseCode: string = encodeToMorse (plaintext)
        let mutable combinedKey: string = (unbox<string> (key.ToUpper())) + "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let mutable dedupKey: string = ""
        let mutable i: int = 0
        while i < (String.length (combinedKey)) do
            let ch: string = combinedKey.Substring(i, (i + 1) - i)
            if not (dedupKey.Contains(ch)) then
                dedupKey <- dedupKey + ch
            i <- i + 1
        let mutable paddingLength: int = 3 - ((((String.length (morseCode)) % 3 + 3) % 3))
        let mutable p: int = 0
        while p < paddingLength do
            morseCode <- morseCode + "x"
            p <- p + 1
        let mutable dict: System.Collections.Generic.IDictionary<string, string> = _dictCreate []
        let mutable j: int = 0
        while j < 26 do
            let combo: string = _idx MORSE_COMBINATIONS (j)
            let letter: string = dedupKey.Substring(j, (j + 1) - j)
            dict.[combo] <- letter
            j <- j + 1
        dict.["xxx"] <- ""
        let mutable encrypted: string = ""
        let mutable k: int = 0
        while k < (String.length (morseCode)) do
            let group: string = morseCode.Substring(k, (k + 3) - k)
            encrypted <- encrypted + (dict.[(string (group))])
            k <- k + 3
        __ret <- encrypted
        raise Return
        __ret
    with
        | Return -> __ret
let rec decryptFractionatedMorse (ciphertext: string) (key: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ciphertext = ciphertext
    let mutable key = key
    try
        let mutable combinedKey: string = (unbox<string> (key.ToUpper())) + "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let mutable dedupKey: string = ""
        let mutable i: int = 0
        while i < (String.length (combinedKey)) do
            let ch: string = combinedKey.Substring(i, (i + 1) - i)
            if not (dedupKey.Contains(ch)) then
                dedupKey <- dedupKey + ch
            i <- i + 1
        let mutable inv: System.Collections.Generic.IDictionary<string, string> = _dictCreate []
        let mutable j: int = 0
        while j < 26 do
            let letter: string = dedupKey.Substring(j, (j + 1) - j)
            inv.[letter] <- _idx MORSE_COMBINATIONS (j)
            j <- j + 1
        let mutable morse: string = ""
        let mutable k: int = 0
        while k < (String.length (ciphertext)) do
            let ch: string = ciphertext.Substring(k, (k + 1) - k)
            if inv.ContainsKey(ch) then
                morse <- morse + (inv.[(string (ch))])
            k <- k + 1
        let mutable codes: string array = [||]
        let mutable current: string = ""
        let mutable m: int = 0
        while m < (String.length (morse)) do
            let ch: string = morse.Substring(m, (m + 1) - m)
            if ch = "x" then
                codes <- Array.append codes [|current|]
                current <- ""
            else
                current <- current + ch
            m <- m + 1
        codes <- Array.append codes [|current|]
        let mutable decrypted: string = ""
        let mutable idx: int = 0
        while idx < (Seq.length (codes)) do
            let mutable code: string = _idx codes (idx)
            decrypted <- decrypted + (REVERSE_DICT.[(string (code))])
            idx <- idx + 1
        let mutable start: int = 0
        try
            while true do
                try
                    if start < (String.length (decrypted)) then
                        if (decrypted.Substring(start, (start + 1) - start)) = " " then
                            start <- start + 1
                            raise Continue
                    raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable ``end``: int = String.length (decrypted)
        try
            while true do
                try
                    if ``end`` > start then
                        if (decrypted.Substring(``end`` - 1, ``end`` - (``end`` - 1))) = " " then
                            ``end`` <- ``end`` - 1
                            raise Continue
                    raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- decrypted.Substring(start, ``end`` - start)
        raise Return
        __ret
    with
        | Return -> __ret
let plaintext: string = "defend the east"
printfn "%s" (String.concat " " ([|sprintf "%s" ("Plain Text:"); sprintf "%s" (plaintext)|]))
let key: string = "ROUNDTABLE"
let ciphertext: string = encryptFractionatedMorse (plaintext) (key)
printfn "%s" (String.concat " " ([|sprintf "%s" ("Encrypted:"); sprintf "%s" (ciphertext)|]))
let mutable decrypted: string = decryptFractionatedMorse (ciphertext) (key)
printfn "%s" (String.concat " " ([|sprintf "%s" ("Decrypted:"); sprintf "%s" (decrypted)|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
