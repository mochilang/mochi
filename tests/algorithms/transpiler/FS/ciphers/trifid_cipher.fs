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

let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let triagrams: string array = [|"111"; "112"; "113"; "121"; "122"; "123"; "131"; "132"; "133"; "211"; "212"; "213"; "221"; "222"; "223"; "231"; "232"; "233"; "311"; "312"; "313"; "321"; "322"; "323"; "331"; "332"; "333"|]
let rec remove_spaces (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = _substring s i (i + 1)
            if c <> " " then
                res <- res + c
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and char_to_trigram (ch: string) (alphabet: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    let mutable alphabet = alphabet
    try
        let mutable i: int = 0
        while i < (String.length (alphabet)) do
            if (_substring alphabet i (i + 1)) = ch then
                __ret <- _idx triagrams (i)
                raise Return
            i <- i + 1
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
and trigram_to_char (tri: string) (alphabet: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable tri = tri
    let mutable alphabet = alphabet
    try
        let mutable i: int = 0
        while i < (Seq.length (triagrams)) do
            if (_idx triagrams (i)) = tri then
                __ret <- _substring alphabet i (i + 1)
                raise Return
            i <- i + 1
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
and encrypt_part (part: string) (alphabet: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable part = part
    let mutable alphabet = alphabet
    try
        let mutable one: string = ""
        let mutable two: string = ""
        let mutable three: string = ""
        let mutable i: int = 0
        while i < (String.length (part)) do
            let tri: string = char_to_trigram (_substring part i (i + 1)) (alphabet)
            one <- one + (_substring tri 0 1)
            two <- two + (_substring tri 1 2)
            three <- three + (_substring tri 2 3)
            i <- i + 1
        __ret <- (one + two) + three
        raise Return
        __ret
    with
        | Return -> __ret
and encrypt_message (message: string) (alphabet: string) (period: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    let mutable alphabet = alphabet
    let mutable period = period
    try
        let msg: string = remove_spaces (message)
        let alpha: string = remove_spaces (alphabet)
        if (String.length (alpha)) <> 27 then
            __ret <- ""
            raise Return
        let mutable encrypted_numeric: string = ""
        let mutable i: int = 0
        while i < (String.length (msg)) do
            let mutable ``end``: int = i + period
            if ``end`` > (String.length (msg)) then
                ``end`` <- String.length (msg)
            let part: string = _substring msg i ``end``
            encrypted_numeric <- encrypted_numeric + (encrypt_part (part) (alpha))
            i <- i + period
        let mutable encrypted: string = ""
        let mutable j: int = 0
        while j < (String.length (encrypted_numeric)) do
            let tri: string = _substring encrypted_numeric j (j + 3)
            encrypted <- encrypted + (trigram_to_char (tri) (alpha))
            j <- j + 3
        __ret <- encrypted
        raise Return
        __ret
    with
        | Return -> __ret
and decrypt_part (part: string) (alphabet: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable part = part
    let mutable alphabet = alphabet
    try
        let mutable converted: string = ""
        let mutable i: int = 0
        while i < (String.length (part)) do
            let tri: string = char_to_trigram (_substring part i (i + 1)) (alphabet)
            converted <- converted + tri
            i <- i + 1
        let mutable result: string array = [||]
        let mutable tmp: string = ""
        let mutable j: int = 0
        while j < (String.length (converted)) do
            tmp <- tmp + (_substring converted j (j + 1))
            if (String.length (tmp)) = (String.length (part)) then
                result <- Array.append result [|tmp|]
                tmp <- ""
            j <- j + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and decrypt_message (message: string) (alphabet: string) (period: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    let mutable alphabet = alphabet
    let mutable period = period
    try
        let msg: string = remove_spaces (message)
        let alpha: string = remove_spaces (alphabet)
        if (String.length (alpha)) <> 27 then
            __ret <- ""
            raise Return
        let mutable decrypted_numeric: string array = [||]
        let mutable i: int = 0
        while i < (String.length (msg)) do
            let mutable ``end``: int = i + period
            if ``end`` > (String.length (msg)) then
                ``end`` <- String.length (msg)
            let part: string = _substring msg i ``end``
            let groups: string array = decrypt_part (part) (alpha)
            let mutable k: int = 0
            while k < (String.length (_idx groups (0))) do
                let tri: string = ((_substring (_idx groups (0)) k (k + 1)) + (_substring (_idx groups (1)) k (k + 1))) + (_substring (_idx groups (2)) k (k + 1))
                decrypted_numeric <- Array.append decrypted_numeric [|tri|]
                k <- k + 1
            i <- i + period
        let mutable decrypted: string = ""
        let mutable j: int = 0
        while j < (Seq.length (decrypted_numeric)) do
            decrypted <- decrypted + (trigram_to_char (_idx decrypted_numeric (j)) (alpha))
            j <- j + 1
        __ret <- decrypted
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let msg: string = "DEFEND THE EAST WALL OF THE CASTLE."
        let alphabet: string = "EPSDUCVWYM.ZLKXNBTFGORIJHAQ"
        let mutable encrypted: string = encrypt_message (msg) (alphabet) (5)
        let mutable decrypted: string = decrypt_message (encrypted) (alphabet) (5)
        printfn "%s" ("Encrypted: " + encrypted)
        printfn "%s" ("Decrypted: " + decrypted)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
