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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let BYTE_SIZE: int = 256
let rec pow_int (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and mod_pow (``base``: int) (exponent: int) (modulus: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exponent = exponent
    let mutable modulus = modulus
    try
        let mutable result: int = 1
        let mutable b: int = ((``base`` % modulus + modulus) % modulus)
        let mutable e: int = exponent
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                result <- (((result * b) % modulus + modulus) % modulus)
            e <- e / 2
            b <- (((b * b) % modulus + modulus) % modulus)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        if ch = " " then
            __ret <- 32
            raise Return
        if ch = "a" then
            __ret <- 97
            raise Return
        if ch = "b" then
            __ret <- 98
            raise Return
        if ch = "c" then
            __ret <- 99
            raise Return
        if ch = "d" then
            __ret <- 100
            raise Return
        if ch = "e" then
            __ret <- 101
            raise Return
        if ch = "f" then
            __ret <- 102
            raise Return
        if ch = "g" then
            __ret <- 103
            raise Return
        if ch = "h" then
            __ret <- 104
            raise Return
        if ch = "i" then
            __ret <- 105
            raise Return
        if ch = "j" then
            __ret <- 106
            raise Return
        if ch = "k" then
            __ret <- 107
            raise Return
        if ch = "l" then
            __ret <- 108
            raise Return
        if ch = "m" then
            __ret <- 109
            raise Return
        if ch = "n" then
            __ret <- 110
            raise Return
        if ch = "o" then
            __ret <- 111
            raise Return
        if ch = "p" then
            __ret <- 112
            raise Return
        if ch = "q" then
            __ret <- 113
            raise Return
        if ch = "r" then
            __ret <- 114
            raise Return
        if ch = "s" then
            __ret <- 115
            raise Return
        if ch = "t" then
            __ret <- 116
            raise Return
        if ch = "u" then
            __ret <- 117
            raise Return
        if ch = "v" then
            __ret <- 118
            raise Return
        if ch = "w" then
            __ret <- 119
            raise Return
        if ch = "x" then
            __ret <- 120
            raise Return
        if ch = "y" then
            __ret <- 121
            raise Return
        if ch = "z" then
            __ret <- 122
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and chr (code: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable code = code
    try
        if code = 32 then
            __ret <- " "
            raise Return
        if code = 97 then
            __ret <- "a"
            raise Return
        if code = 98 then
            __ret <- "b"
            raise Return
        if code = 99 then
            __ret <- "c"
            raise Return
        if code = 100 then
            __ret <- "d"
            raise Return
        if code = 101 then
            __ret <- "e"
            raise Return
        if code = 102 then
            __ret <- "f"
            raise Return
        if code = 103 then
            __ret <- "g"
            raise Return
        if code = 104 then
            __ret <- "h"
            raise Return
        if code = 105 then
            __ret <- "i"
            raise Return
        if code = 106 then
            __ret <- "j"
            raise Return
        if code = 107 then
            __ret <- "k"
            raise Return
        if code = 108 then
            __ret <- "l"
            raise Return
        if code = 109 then
            __ret <- "m"
            raise Return
        if code = 110 then
            __ret <- "n"
            raise Return
        if code = 111 then
            __ret <- "o"
            raise Return
        if code = 112 then
            __ret <- "p"
            raise Return
        if code = 113 then
            __ret <- "q"
            raise Return
        if code = 114 then
            __ret <- "r"
            raise Return
        if code = 115 then
            __ret <- "s"
            raise Return
        if code = 116 then
            __ret <- "t"
            raise Return
        if code = 117 then
            __ret <- "u"
            raise Return
        if code = 118 then
            __ret <- "v"
            raise Return
        if code = 119 then
            __ret <- "w"
            raise Return
        if code = 120 then
            __ret <- "x"
            raise Return
        if code = 121 then
            __ret <- "y"
            raise Return
        if code = 122 then
            __ret <- "z"
            raise Return
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
and get_blocks_from_text (message: string) (block_size: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable message = message
    let mutable block_size = block_size
    try
        let mutable block_ints: int array = [||]
        let mutable block_start: int = 0
        while block_start < (String.length (message)) do
            let mutable block_int: int = 0
            let mutable i: int = block_start
            while (i < (block_start + block_size)) && (i < (String.length (message))) do
                block_int <- block_int + ((ord (string (message.[i]))) * (pow_int (BYTE_SIZE) (i - block_start)))
                i <- i + 1
            block_ints <- Array.append block_ints [|block_int|]
            block_start <- block_start + block_size
        __ret <- block_ints
        raise Return
        __ret
    with
        | Return -> __ret
and get_text_from_blocks (block_ints: int array) (message_length: int) (block_size: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable block_ints = block_ints
    let mutable message_length = message_length
    let mutable block_size = block_size
    try
        let mutable message: string = ""
        for block_int in block_ints do
            let mutable block: int = block_int
            let mutable i: int = block_size - 1
            let mutable block_message: string = ""
            while i >= 0 do
                if ((String.length (message)) + i) < message_length then
                    let ascii_number: int = block / (pow_int (BYTE_SIZE) (i))
                    block <- ((block % (pow_int (BYTE_SIZE) (i)) + (pow_int (BYTE_SIZE) (i))) % (pow_int (BYTE_SIZE) (i)))
                    block_message <- (chr (ascii_number)) + block_message
                i <- i - 1
            message <- message + block_message
        __ret <- message
        raise Return
        __ret
    with
        | Return -> __ret
and encrypt_message (message: string) (n: int) (e: int) (block_size: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable message = message
    let mutable n = n
    let mutable e = e
    let mutable block_size = block_size
    try
        let mutable encrypted: int array = [||]
        let blocks: int array = get_blocks_from_text (message) (block_size)
        for block in blocks do
            encrypted <- Array.append encrypted [|mod_pow (block) (e) (n)|]
        __ret <- encrypted
        raise Return
        __ret
    with
        | Return -> __ret
and decrypt_message (blocks: int array) (message_length: int) (n: int) (d: int) (block_size: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable blocks = blocks
    let mutable message_length = message_length
    let mutable n = n
    let mutable d = d
    let mutable block_size = block_size
    try
        let mutable decrypted_blocks: int array = [||]
        for block in blocks do
            decrypted_blocks <- Array.append decrypted_blocks [|mod_pow (block) (d) (n)|]
        let mutable message: string = ""
        for num in decrypted_blocks do
            message <- message + (chr (num))
        __ret <- message
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable message: string = "hello world"
        let n: int = 3233
        let mutable e: int = 17
        let d: int = 2753
        let block_size: int = 1
        let mutable encrypted: int array = encrypt_message (message) (n) (e) (block_size)
        printfn "%s" (_str (encrypted))
        let decrypted: string = decrypt_message (encrypted) (String.length (message)) (n) (d) (block_size)
        printfn "%s" (decrypted)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
