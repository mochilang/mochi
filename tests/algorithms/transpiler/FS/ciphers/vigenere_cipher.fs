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
let LETTERS: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let LETTERS_LOWER: string = "abcdefghijklmnopqrstuvwxyz"
let rec find_index (s: string) (ch: string) =
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
let rec to_upper_char (ch: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    try
        let idx: int = find_index (LETTERS_LOWER) (ch)
        if idx >= 0 then
            __ret <- string (LETTERS.[idx])
            raise Return
        __ret <- ch
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_lower_char (ch: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    try
        let idx: int = find_index (LETTERS) (ch)
        if idx >= 0 then
            __ret <- string (LETTERS_LOWER.[idx])
            raise Return
        __ret <- ch
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_upper (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        __ret <- (find_index (LETTERS) (ch)) >= 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_upper_string (s: string) =
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
let key: string = "HDarji"
let message: string = "This is Harshil Darji from Dharmaj."
let key_up: string = to_upper_string (key)
let mutable encrypted: string = ""
let mutable key_index: int = 0
let mutable i: int = 0
while i < (String.length (message)) do
    let symbol: string = string (message.[i])
    let upper_symbol: string = to_upper_char (symbol)
    let mutable num: int = find_index (LETTERS) (upper_symbol)
    if num >= 0 then
        num <- num + (find_index (LETTERS) (string (key_up.[key_index])))
        num <- ((num % (String.length (LETTERS)) + (String.length (LETTERS))) % (String.length (LETTERS)))
        if is_upper (symbol) then
            encrypted <- encrypted + (string (LETTERS.[num]))
        else
            encrypted <- encrypted + (to_lower_char (string (LETTERS.[num])))
        key_index <- key_index + 1
        if key_index = (String.length (key_up)) then
            key_index <- 0
    else
        encrypted <- encrypted + symbol
    i <- i + 1
printfn "%s" (encrypted)
let mutable decrypted: string = ""
key_index <- 0
i <- 0
while i < (String.length (encrypted)) do
    let symbol: string = string (encrypted.[i])
    let upper_symbol: string = to_upper_char (symbol)
    let mutable num: int = find_index (LETTERS) (upper_symbol)
    if num >= 0 then
        num <- num - (find_index (LETTERS) (string (key_up.[key_index])))
        num <- ((num % (String.length (LETTERS)) + (String.length (LETTERS))) % (String.length (LETTERS)))
        if is_upper (symbol) then
            decrypted <- decrypted + (string (LETTERS.[num]))
        else
            decrypted <- decrypted + (to_lower_char (string (LETTERS.[num])))
        key_index <- key_index + 1
        if key_index = (String.length (key_up)) then
            key_index <- 0
    else
        decrypted <- decrypted + symbol
    i <- i + 1
printfn "%s" (decrypted)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
