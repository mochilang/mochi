// Generated 2025-08-07 15:46 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec longest_subsequence (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let n: int = Seq.length (xs)
        if n <= 1 then
            __ret <- xs
            raise Return
        let pivot: int = _idx xs (0)
        let mutable is_found: bool = false
        let mutable i: int = 1
        let mutable longest_subseq: int array = [||]
        while (not is_found) && (i < n) do
            if (_idx xs (i)) < pivot then
                is_found <- true
                let mutable temp_array: int array = Array.sub xs i (n - i)
                temp_array <- longest_subsequence (temp_array)
                if (Seq.length (temp_array)) > (Seq.length (longest_subseq)) then
                    longest_subseq <- temp_array
            else
                i <- i + 1
        let mutable filtered: int array = [||]
        let mutable j: int = 1
        while j < n do
            if (_idx xs (j)) >= pivot then
                filtered <- Array.append filtered [|_idx xs (j)|]
            j <- j + 1
        let mutable candidate: int array = [||]
        candidate <- Array.append candidate [|pivot|]
        candidate <- unbox<int array> (Array.append (candidate) (longest_subsequence (filtered)))
        if (Seq.length (candidate)) > (Seq.length (longest_subseq)) then
            __ret <- candidate
            raise Return
        else
            __ret <- longest_subseq
            raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
