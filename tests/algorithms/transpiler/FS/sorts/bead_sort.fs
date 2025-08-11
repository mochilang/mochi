// Generated 2025-08-11 16:20 +0700

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
let rec bead_sort (sequence: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable sequence = sequence
    try
        let n: int = Seq.length (sequence)
        let mutable i: int = 0
        while i < n do
            if (_idx sequence (int i)) < 0 then
                failwith ("Sequence must be list of non-negative integers")
            i <- i + 1
        let mutable pass: int = 0
        while pass < n do
            let mutable j: int = 0
            while j < (n - 1) do
                let upper: int = _idx sequence (int j)
                let lower: int = _idx sequence (int (j + 1))
                if upper > lower then
                    let diff: int = upper - lower
                    sequence.[int j] <- upper - diff
                    sequence.[int (j + 1)] <- lower + diff
                j <- j + 1
            pass <- pass + 1
        __ret <- sequence
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (bead_sort (unbox<int array> [|6; 11; 12; 4; 1; 5|])))
printfn "%s" (_str (bead_sort (unbox<int array> [|9; 8; 7; 6; 5; 4; 3; 2; 1|])))
printfn "%s" (_str (bead_sort (unbox<int array> [|5; 0; 4; 3|])))
printfn "%s" (_str (bead_sort (unbox<int array> [|8; 2; 1|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
