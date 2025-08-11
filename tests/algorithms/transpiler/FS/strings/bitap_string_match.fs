// Generated 2025-08-11 17:23 +0700

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
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
let rec pow2 (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable res: int = 1
        let mutable i: int = 0
        while i < n do
            res <- int ((int64 res) * (int64 2))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and bit_and (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        let mutable res: int = 0
        let mutable bit: int = 1
        while (x > 0) || (y > 0) do
            if ((((x % 2 + 2) % 2)) = 1) && ((((y % 2 + 2) % 2)) = 1) then
                res <- res + bit
            x <- int (_floordiv x 2)
            y <- int (_floordiv y 2)
            bit <- int ((int64 bit) * (int64 2))
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and bit_or (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        let mutable res: int = 0
        let mutable bit: int = 1
        while (x > 0) || (y > 0) do
            if ((((x % 2 + 2) % 2)) = 1) || ((((y % 2 + 2) % 2)) = 1) then
                res <- res + bit
            x <- int (_floordiv x 2)
            y <- int (_floordiv y 2)
            bit <- int ((int64 bit) * (int64 2))
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and char_to_index (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let letters: string = "abcdefghijklmnopqrstuvwxyz"
        let mutable i: int = 0
        while i < (String.length (letters)) do
            if (_substring letters (i) (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- 26
        raise Return
        __ret
    with
        | Return -> __ret
and bitap_string_match (text: string) (pattern: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable text = text
    let mutable pattern = pattern
    try
        if pattern = "" then
            __ret <- 0
            raise Return
        let m: int = String.length (pattern)
        if m > (String.length (text)) then
            __ret <- -1
            raise Return
        let limit: int = pow2 (m + 1)
        let all_ones: int = limit - 1
        let mutable pattern_mask: int array = Array.empty<int>
        let mutable i: int = 0
        while i < 27 do
            pattern_mask <- Array.append pattern_mask [|all_ones|]
            i <- i + 1
        i <- 0
        while i < m do
            let ch: string = _substring pattern (i) (i + 1)
            let idx: int = char_to_index (ch)
            pattern_mask.[int idx] <- bit_and (_idx pattern_mask (int idx)) (all_ones - (pow2 (i)))
            i <- i + 1
        let mutable state: int = all_ones - 1
        i <- 0
        while i < (String.length (text)) do
            let ch: string = _substring text (i) (i + 1)
            let idx: int = char_to_index (ch)
            state <- bit_or (state) (_idx pattern_mask (int idx))
            state <- int (((((int64 state) * (int64 2)) % (int64 limit) + (int64 limit)) % (int64 limit)))
            if (bit_and (state) (pow2 (m))) = 0 then
                __ret <- (i - m) + 1
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (_str (bitap_string_match ("abdabababc") ("ababc")))
        printfn "%s" (_str (bitap_string_match ("abdabababc") ("")))
        printfn "%s" (_str (bitap_string_match ("abdabababc") ("c")))
        printfn "%s" (_str (bitap_string_match ("abdabababc") ("fofosdfo")))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
