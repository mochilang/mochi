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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec ceil_index (v: int array) (left: int) (right: int) (key: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable v = v
    let mutable left = left
    let mutable right = right
    let mutable key = key
    try
        let mutable l: int = left
        let mutable r: int = right
        while (r - l) > 1 do
            let middle: int = (l + r) / 2
            if (_idx v (middle)) >= key then
                r <- middle
            else
                l <- middle
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and longest_increasing_subsequence_length (v: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable v = v
    try
        if (Seq.length (v)) = 0 then
            __ret <- 0
            raise Return
        let mutable tail: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (v)) do
            tail <- Array.append tail [|0|]
            i <- i + 1
        let mutable length: int = 1
        tail.[0] <- _idx v (0)
        let mutable j: int = 1
        while j < (Seq.length (v)) do
            if (_idx v (j)) < (_idx tail (0)) then
                tail.[0] <- _idx v (j)
            else
                if (_idx v (j)) > (_idx tail (length - 1)) then
                    tail.[length] <- _idx v (j)
                    length <- length + 1
                else
                    let idx: int = ceil_index (tail) (-1) (length - 1) (_idx v (j))
                    tail.[idx] <- _idx v (j)
            j <- j + 1
        __ret <- length
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let example1: int array = [|2; 5; 3; 7; 11; 8; 10; 13; 6|]
        let example2: int array = [||]
        let example3: int array = [|0; 8; 4; 12; 2; 10; 6; 14; 1; 9; 5; 13; 3; 11; 7; 15|]
        let example4: int array = [|5; 4; 3; 2; 1|]
        printfn "%d" (longest_increasing_subsequence_length (example1))
        printfn "%d" (longest_increasing_subsequence_length (example2))
        printfn "%d" (longest_increasing_subsequence_length (example3))
        printfn "%d" (longest_increasing_subsequence_length (example4))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
