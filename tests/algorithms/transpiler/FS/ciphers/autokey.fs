// Generated 2025-08-06 23:33 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let LOWER: string = "abcdefghijklmnopqrstuvwxyz"
let UPPER: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec to_lowercase (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable res: string = ""
        let mutable i: int = 0
        try
            while i < (String.length (s)) do
                try
                    let c: string = string (s.[i])
                    let mutable j: int = 0
                    let mutable found: bool = false
                    try
                        while j < 26 do
                            try
                                if c = (string (UPPER.[j])) then
                                    res <- res + (string (LOWER.[j]))
                                    found <- true
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if not found then
                        res <- res + c
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and char_index (c: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable c = c
    try
        let mutable i: int = 0
        while i < 26 do
            if c = (string (LOWER.[i])) then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and index_char (i: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable i = i
    try
        __ret <- string (LOWER.[i])
        raise Return
        __ret
    with
        | Return -> __ret
and encrypt (plaintext: string) (key: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable plaintext = plaintext
    let mutable key = key
    try
        if (String.length (plaintext)) = 0 then
            failwith ("plaintext is empty")
        if (String.length (key)) = 0 then
            failwith ("key is empty")
        let mutable full_key: string = key + plaintext
        plaintext <- to_lowercase (plaintext)
        full_key <- to_lowercase (full_key)
        let mutable p_i: int = 0
        let mutable k_i: int = 0
        let mutable ciphertext: string = ""
        while p_i < (String.length (plaintext)) do
            let p_char: string = string (plaintext.[p_i])
            let p_idx: int = char_index (p_char)
            if p_idx < 0 then
                ciphertext <- ciphertext + p_char
                p_i <- p_i + 1
            else
                let k_char: string = string (full_key.[k_i])
                let k_idx: int = char_index (k_char)
                if k_idx < 0 then
                    k_i <- k_i + 1
                else
                    let c_idx: int = (((p_idx + k_idx) % 26 + 26) % 26)
                    ciphertext <- ciphertext + (index_char (c_idx))
                    k_i <- k_i + 1
                    p_i <- p_i + 1
        __ret <- ciphertext
        raise Return
        __ret
    with
        | Return -> __ret
and decrypt (ciphertext: string) (key: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ciphertext = ciphertext
    let mutable key = key
    try
        if (String.length (ciphertext)) = 0 then
            failwith ("ciphertext is empty")
        if (String.length (key)) = 0 then
            failwith ("key is empty")
        let mutable current_key: string = to_lowercase (key)
        let mutable c_i: int = 0
        let mutable k_i: int = 0
        let mutable plaintext: string = ""
        while c_i < (String.length (ciphertext)) do
            let c_char: string = string (ciphertext.[c_i])
            let c_idx: int = char_index (c_char)
            if c_idx < 0 then
                plaintext <- plaintext + c_char
            else
                let k_char: string = string (current_key.[k_i])
                let k_idx: int = char_index (k_char)
                let p_idx: int = ((((c_idx - k_idx) + 26) % 26 + 26) % 26)
                let p_char: string = index_char (p_idx)
                plaintext <- plaintext + p_char
                current_key <- current_key + p_char
                k_i <- k_i + 1
            c_i <- c_i + 1
        __ret <- plaintext
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (encrypt ("hello world") ("coffee"))
printfn "%s" (decrypt ("jsqqs avvwo") ("coffee"))
printfn "%s" (encrypt ("coffee is good as python") ("TheAlgorithms"))
printfn "%s" (decrypt ("vvjfpk wj ohvp su ddylsv") ("TheAlgorithms"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
