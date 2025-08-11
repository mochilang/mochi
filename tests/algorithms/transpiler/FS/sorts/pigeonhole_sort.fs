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
let rec pigeonhole_sort (arr: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    try
        if (Seq.length (arr)) = 0 then
            __ret <- arr
            raise Return
        let min_val: int = int (Seq.min (arr))
        let max_val: int = int (Seq.max (arr))
        let size: int = (max_val - min_val) + 1
        let mutable holes: int array = Array.empty<int>
        let mutable i: int = 0
        while i < size do
            holes <- Array.append holes [|0|]
            i <- i + 1
        i <- 0
        while i < (Seq.length (arr)) do
            let x: int = _idx arr (int i)
            let index: int = x - min_val
            holes.[int index] <- (_idx holes (int index)) + 1
            i <- i + 1
        let mutable sorted_index: int = 0
        let mutable count: int = 0
        while count < size do
            while (_idx holes (int count)) > 0 do
                arr.[int sorted_index] <- count + min_val
                holes.[int count] <- (_idx holes (int count)) - 1
                sorted_index <- sorted_index + 1
            count <- count + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let example: int array = unbox<int array> [|8; 3; 2; 7; 4; 6; 8|]
let result: int array = pigeonhole_sort (example)
let mutable output: string = "Sorted order is:"
let mutable j: int = 0
while j < (Seq.length (result)) do
    output <- (output + " ") + (_str (_idx result (int j)))
    j <- j + 1
printfn "%s" (output)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
