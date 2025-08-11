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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec odd_even_transposition (arr: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable arr = arr
    try
        let n: int = Seq.length (arr)
        let mutable pass: int = 0
        while pass < n do
            let mutable i: int = ((pass % 2 + 2) % 2)
            while i < (n - 1) do
                if (_idx arr (int (i + 1))) < (_idx arr (int i)) then
                    let tmp: float = _idx arr (int i)
                    arr.[int i] <- _idx arr (int (i + 1))
                    arr.[int (i + 1)] <- tmp
                i <- i + 2
            pass <- pass + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (odd_even_transposition (unbox<float array> [|5.0; 4.0; 3.0; 2.0; 1.0|])))
printfn "%s" (_str (odd_even_transposition (unbox<float array> [|13.0; 11.0; 18.0; 0.0; -1.0|])))
printfn "%s" (_str (odd_even_transposition (unbox<float array> [|-0.1; 1.1; 0.1; -2.9|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
