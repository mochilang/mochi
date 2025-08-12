// Generated 2025-08-12 13:41 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec get_totients (max_one: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable max_one = max_one
    try
        let mutable totients: int array = Array.empty<int>
        let mutable i: int = 0
        while i < max_one do
            totients <- Array.append totients [|i|]
            i <- i + 1
        i <- 2
        while i < max_one do
            if (_idx totients (int i)) = i then
                let mutable x: int = i
                while x < max_one do
                    totients.[x] <- (_idx totients (int x)) - (_floordiv (_idx totients (int x)) i)
                    x <- x + i
            i <- i + 1
        __ret <- totients
        raise Return
        __ret
    with
        | Return -> __ret
and has_same_digits (num1: int) (num2: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable num1 = num1
    let mutable num2 = num2
    try
        let mutable count1: int array = Array.empty<int>
        let mutable count2: int array = Array.empty<int>
        let mutable i: int = 0
        while i < 10 do
            count1 <- Array.append count1 [|0|]
            count2 <- Array.append count2 [|0|]
            i <- i + 1
        let mutable n1: int = num1
        let mutable n2: int = num2
        if n1 = 0 then
            count1.[0] <- (_idx count1 (int 0)) + 1
        if n2 = 0 then
            count2.[0] <- (_idx count2 (int 0)) + 1
        while n1 > 0 do
            let d1: int = ((n1 % 10 + 10) % 10)
            count1.[d1] <- (_idx count1 (int d1)) + 1
            n1 <- _floordiv n1 10
        while n2 > 0 do
            let d2: int = ((n2 % 10 + 10) % 10)
            count2.[d2] <- (_idx count2 (int d2)) + 1
            n2 <- _floordiv n2 10
        i <- 0
        while i < 10 do
            if (_idx count1 (int i)) <> (_idx count2 (int i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and solution (max_n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable max_n = max_n
    try
        let mutable min_numerator: int = 1
        let mutable min_denominator: int = 0
        let mutable totients: int array = get_totients (max_n + 1)
        let mutable i: int = 2
        while i <= max_n do
            let t: int = _idx totients (int i)
            if ((i * min_denominator) < (min_numerator * t)) && (has_same_digits (i) (t)) then
                min_numerator <- i
                min_denominator <- t
            i <- i + 1
        __ret <- min_numerator
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (solution (10000))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
