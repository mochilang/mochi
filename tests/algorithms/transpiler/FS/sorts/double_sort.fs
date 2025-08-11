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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec double_sort (collection: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable collection = collection
    try
        let no_of_elements: int = Seq.length (collection)
        let passes: int = (_floordiv (no_of_elements - 1) 2) + 1
        let mutable i: int = 0
        while i < passes do
            let mutable j: int = 0
            while j < (no_of_elements - 1) do
                if (_idx collection (int (j + 1))) < (_idx collection (int j)) then
                    let tmp: int = _idx collection (int j)
                    collection.[int j] <- _idx collection (int (j + 1))
                    collection.[int (j + 1)] <- tmp
                if (_idx collection (int ((no_of_elements - 1) - j))) < (_idx collection (int ((no_of_elements - 2) - j))) then
                    let tmp2: int = _idx collection (int ((no_of_elements - 1) - j))
                    collection.[int ((no_of_elements - 1) - j)] <- _idx collection (int ((no_of_elements - 2) - j))
                    collection.[int ((no_of_elements - 2) - j)] <- tmp2
                j <- j + 1
            i <- i + 1
        __ret <- collection
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (double_sort (unbox<int array> [|-1; -2; -3; -4; -5; -6; -7|])))
printfn "%s" (_str (double_sort (Array.empty<int>)))
printfn "%s" (_str (double_sort (unbox<int array> [|-1; -2; -3; -4; -5; -6|])))
printfn "%s" (_str ((double_sort (unbox<int array> [|-3; 10; 16; -42; 29|])) = [|-42; -3; 10; 16; 29|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
