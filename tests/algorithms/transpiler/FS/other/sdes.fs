// Generated 2025-08-09 16:21 +0700

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
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec apply_table (inp: string) (table: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable inp = inp
    let mutable table = table
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length (table)) do
            let mutable idx: int = (_idx table (int i)) - 1
            if idx < 0 then
                idx <- (String.length (inp)) - 1
            res <- res + (_substring inp (idx) (idx + 1))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec left_shift (data: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable data = data
    try
        __ret <- (_substring data (1) (String.length (data))) + (_substring data (0) (1))
        raise Return
        __ret
    with
        | Return -> __ret
let rec xor (a: string) (b: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while (i < (String.length (a))) && (i < (String.length (b))) do
            if (_substring a (i) (i + 1)) = (_substring b (i) (i + 1)) then
                res <- res + "0"
            else
                res <- res + "1"
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec int_to_binary (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n = 0 then
            __ret <- "0"
            raise Return
        let mutable res: string = ""
        let mutable num: int = n
        while num > 0 do
            res <- (_str (((num % 2 + 2) % 2))) + res
            num <- _floordiv num 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec pad_left (s: string) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable width = width
    try
        let mutable res: string = s
        while (String.length (res)) < width do
            res <- "0" + res
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec bin_to_int (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable result: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            let digit: int = int (_substring s (i) (i + 1))
            result <- int (((int64 result) * (int64 2)) + (int64 digit))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec apply_sbox (s: int array array) (data: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable data = data
    try
        let row_bits: string = (_substring data (0) (1)) + (_substring data ((String.length (data)) - 1) (String.length (data)))
        let col_bits: string = _substring data (1) (3)
        let row: int = bin_to_int (row_bits)
        let col: int = bin_to_int (col_bits)
        let ``val``: int = _idx (_idx s (int row)) (int col)
        let out: string = int_to_binary (``val``)
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let p4_table: int array = unbox<int array> [|2; 4; 3; 1|]
let rec f (expansion: int array) (s0: int array array) (s1: int array array) (key: string) (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable expansion = expansion
    let mutable s0 = s0
    let mutable s1 = s1
    let mutable key = key
    let mutable message = message
    try
        let mutable left: string = _substring message (0) (4)
        let mutable right: string = _substring message (4) (8)
        let mutable temp: string = apply_table (right) (expansion)
        temp <- xor (temp) (key)
        let mutable left_bin_str: string = apply_sbox (s0) (_substring temp (0) (4))
        let mutable right_bin_str: string = apply_sbox (s1) (_substring temp (4) (8))
        left_bin_str <- pad_left (left_bin_str) (2)
        right_bin_str <- pad_left (right_bin_str) (2)
        temp <- apply_table (left_bin_str + right_bin_str) (p4_table)
        temp <- xor (left) (temp)
        __ret <- temp + right
        raise Return
        __ret
    with
        | Return -> __ret
let key: string = "1010000010"
let message: string = "11010111"
let p8_table: int array = unbox<int array> [|6; 3; 7; 4; 8; 5; 10; 9|]
let p10_table: int array = unbox<int array> [|3; 5; 2; 7; 4; 10; 1; 9; 8; 6|]
let IP: int array = unbox<int array> [|2; 6; 3; 1; 4; 8; 5; 7|]
let IP_inv: int array = unbox<int array> [|4; 1; 3; 5; 7; 2; 8; 6|]
let expansion: int array = unbox<int array> [|4; 1; 2; 3; 2; 3; 4; 1|]
let s0: int array array = [|[|1; 0; 3; 2|]; [|3; 2; 1; 0|]; [|0; 2; 1; 3|]; [|3; 1; 3; 2|]|]
let s1: int array array = [|[|0; 1; 2; 3|]; [|2; 0; 1; 3|]; [|3; 0; 1; 0|]; [|2; 1; 0; 3|]|]
let mutable temp: string = apply_table (key) (p10_table)
let mutable left: string = _substring temp (0) (5)
let mutable right: string = _substring temp (5) (10)
left <- left_shift (left)
right <- left_shift (right)
let key1: string = apply_table (left + right) (p8_table)
left <- left_shift (left)
right <- left_shift (right)
left <- left_shift (left)
right <- left_shift (right)
let key2: string = apply_table (left + right) (p8_table)
temp <- apply_table (message) (IP)
temp <- f (expansion) (s0) (s1) (key1) (temp)
temp <- (_substring temp (4) (8)) + (_substring temp (0) (4))
temp <- f (expansion) (s0) (s1) (key2) (temp)
let CT: string = apply_table (temp) (IP_inv)
printfn "%s" ("Cipher text is: " + CT)
temp <- apply_table (CT) (IP)
temp <- f (expansion) (s0) (s1) (key2) (temp)
temp <- (_substring temp (4) (8)) + (_substring temp (0) (4))
temp <- f (expansion) (s0) (s1) (key1) (temp)
let PT: string = apply_table (temp) (IP_inv)
printfn "%s" ("Plain text after decypting is: " + PT)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
