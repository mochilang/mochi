// Generated 2025-08-06 21:33 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec pow2 (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < exp do
            result <- result * 2
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and gray_code (bit_count: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable bit_count = bit_count
    try
        if bit_count = 0 then
            __ret <- unbox<int array> [|0|]
            raise Return
        let prev: int array = gray_code (bit_count - 1)
        let add_val: int = pow2 (bit_count - 1)
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (prev)) do
            res <- Array.append res [|_idx prev (i)|]
            i <- i + 1
        let mutable j: int = (Seq.length (prev)) - 1
        while j >= 0 do
            res <- Array.append res [|(_idx prev (j)) + add_val|]
            j <- j - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let seq2: int array = gray_code (2)
printfn "%s" (_str (seq2))
let seq1: int array = gray_code (1)
printfn "%s" (_str (seq1))
let seq3: int array = gray_code (3)
printfn "%s" (_str (seq3))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
