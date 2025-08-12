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
let rec digits_count (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable counts: int array = Array.empty<int>
        let mutable i: int = 0
        while i < 10 do
            counts <- Array.append counts [|0|]
            i <- i + 1
        let mutable x: int = n
        if x = 0 then
            counts.[0] <- (_idx counts (int 0)) + 1
        while x > 0 do
            let d: int = ((x % 10 + 10) % 10)
            counts.[d] <- (_idx counts (int d)) + 1
            x <- _floordiv x 10
        __ret <- counts
        raise Return
        __ret
    with
        | Return -> __ret
and equal_lists (a: int array) (b: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            if (_idx a (int i)) <> (_idx b (int i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and solution () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        let mutable i: int = 1
        while true do
            let c: int array = digits_count (i)
            if ((((equal_lists (c) (digits_count (2 * i))) && (equal_lists (c) (digits_count (3 * i)))) && (equal_lists (c) (digits_count (4 * i)))) && (equal_lists (c) (digits_count (5 * i)))) && (equal_lists (c) (digits_count (6 * i))) then
                __ret <- i
                raise Return
            i <- i + 1
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (solution())))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
