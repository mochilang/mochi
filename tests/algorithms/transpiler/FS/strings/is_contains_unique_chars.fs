// Generated 2025-08-11 15:32 +0700

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
let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let digits: string = "0123456789"
        let mutable i: int = 0
        while i < (String.length (lower)) do
            if (string (lower.[i])) = ch then
                __ret <- 97 + i
                raise Return
            i <- i + 1
        i <- 0
        while i < (String.length (upper)) do
            if (string (upper.[i])) = ch then
                __ret <- 65 + i
                raise Return
            i <- i + 1
        i <- 0
        while i < (String.length (digits)) do
            if (string (digits.[i])) = ch then
                __ret <- 48 + i
                raise Return
            i <- i + 1
        if ch = " " then
            __ret <- 32
            raise Return
        if ch = "_" then
            __ret <- 95
            raise Return
        if ch = "." then
            __ret <- 46
            raise Return
        if ch = "'" then
            __ret <- 39
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec lshift (num: int) (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    let mutable k = k
    try
        let mutable result: int = num
        let mutable i: int = 0
        while i < k do
            result <- int ((int64 result) * (int64 2))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec rshift (num: int) (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    let mutable k = k
    try
        let mutable result: int = num
        let mutable i: int = 0
        while i < k do
            result <- _floordiv (result - (((result % 2 + 2) % 2))) 2
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_contains_unique_chars (input_str: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable input_str = input_str
    try
        let mutable bitmap: int = 0
        let mutable i: int = 0
        while i < (String.length (input_str)) do
            let code: int = ord (string (input_str.[i]))
            if ((((rshift (bitmap) (code)) % 2 + 2) % 2)) = 1 then
                __ret <- false
                raise Return
            bitmap <- bitmap + (lshift (1) (code))
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (is_contains_unique_chars ("I_love.py")))
printfn "%s" (_str (is_contains_unique_chars ("I don't love Python")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
