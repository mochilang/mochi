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
let rec stooge (arr: int array) (i: int) (h: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable arr = arr
    let mutable i = i
    let mutable h = h
    try
        if i >= h then
            __ret <- ()
            raise Return
        if (_idx arr (int i)) > (_idx arr (int h)) then
            let tmp: int = _idx arr (int i)
            arr.[int i] <- _idx arr (int h)
            arr.[int h] <- tmp
        if ((h - i) + 1) > 2 then
            let t: int = int (_floordiv ((h - i) + 1) 3)
            stooge (arr) (i) (h - t)
            stooge (arr) (i + t) (h)
            stooge (arr) (i) (h - t)
        __ret
    with
        | Return -> __ret
and stooge_sort (arr: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    try
        stooge (arr) (0) ((Seq.length (arr)) - 1)
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (stooge_sort (unbox<int array> [|18; 0; -7; -1; 2; 2|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
