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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let mutable seed: int = 1
let rec rand (max: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable max = max
    try
        seed <- ((((seed * 1103515245) + 12345) % 2147483647 + 2147483647) % 2147483647)
        __ret <- ((seed % max + max) % max)
        raise Return
        __ret
    with
        | Return -> __ret
let rec generate_valid_block_size (message_length: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable message_length = message_length
    try
        let mutable factors: int array = [||]
        let mutable i: int = 2
        while i <= message_length do
            if (((message_length % i + i) % i)) = 0 then
                factors <- Array.append factors [|i|]
            i <- i + 1
        let idx: int = rand (Seq.length (factors))
        __ret <- _idx factors (idx)
        raise Return
        __ret
    with
        | Return -> __ret
let rec generate_permutation_key (block_size: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable block_size = block_size
    try
        let mutable digits: int array = [||]
        let mutable i: int = 0
        while i < block_size do
            digits <- Array.append digits [|i|]
            i <- i + 1
        let mutable j: int = block_size - 1
        while j > 0 do
            let k: int = rand (j + 1)
            let temp: int = _idx digits (j)
            digits.[j] <- _idx digits (k)
            digits.[k] <- temp
            j <- j - 1
        __ret <- digits
        raise Return
        __ret
    with
        | Return -> __ret
let rec encrypt (message: string) (key: int array) (block_size: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    let mutable key = key
    let mutable block_size = block_size
    try
        let mutable encrypted: string = ""
        let mutable i: int = 0
        while i < (String.length (message)) do
            let block: string = _substring message i (i + block_size)
            let mutable j: int = 0
            while j < block_size do
                encrypted <- encrypted + (_substring block (_idx key (j)) ((_idx key (j)) + 1))
                j <- j + 1
            i <- i + block_size
        __ret <- encrypted
        raise Return
        __ret
    with
        | Return -> __ret
let rec repeat_string (times: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable times = times
    try
        let mutable res: string array = [||]
        let mutable i: int = 0
        while i < times do
            res <- Array.append res [|""|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec decrypt (encrypted: string) (key: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable encrypted = encrypted
    let mutable key = key
    try
        let klen: int = Seq.length (key)
        let mutable decrypted: string = ""
        let mutable i: int = 0
        while i < (String.length (encrypted)) do
            let block: string = _substring encrypted i (i + klen)
            let mutable original: string array = repeat_string (klen)
            let mutable j: int = 0
            while j < klen do
                original.[_idx key (j)] <- _substring block j (j + 1)
                j <- j + 1
            j <- 0
            while j < klen do
                decrypted <- decrypted + (_idx original (j))
                j <- j + 1
            i <- i + klen
        __ret <- decrypted
        raise Return
        __ret
    with
        | Return -> __ret
let message: string = "HELLO WORLD"
let block_size: int = generate_valid_block_size (String.length (message))
let key: int array = generate_permutation_key (block_size)
let mutable encrypted: string = encrypt (message) (key) (block_size)
let mutable decrypted: string = decrypt (encrypted) (key)
printfn "%s" ("Block size: " + (_str (block_size)))
printfn "%s" ("Key: " + (_str (key)))
printfn "%s" ("Encrypted: " + encrypted)
printfn "%s" ("Decrypted: " + decrypted)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
