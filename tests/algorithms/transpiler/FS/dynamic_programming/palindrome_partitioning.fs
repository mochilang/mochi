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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec min_partitions (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let n: int = String.length (s)
        let mutable cut: int array = [||]
        let mutable i: int = 0
        while i < n do
            cut <- Array.append cut [|0|]
            i <- i + 1
        let mutable pal: bool array array = [||]
        i <- 0
        while i < n do
            let mutable row: bool array = [||]
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|false|]
                j <- j + 1
            pal <- Array.append pal [|row|]
            i <- i + 1
        i <- 0
        while i < n do
            let mutable mincut: int = i
            let mutable j: int = 0
            while j <= i do
                if ((string (s.[i])) = (string (s.[j]))) && (((i - j) < 2) || (_idx (_idx pal (j + 1)) (i - 1))) then
                    pal.[j].[i] <- true
                    if j = 0 then
                        mincut <- 0
                    else
                        let candidate: int = (_idx cut (j - 1)) + 1
                        if candidate < mincut then
                            mincut <- candidate
                j <- j + 1
            cut.[i] <- mincut
            i <- i + 1
        __ret <- _idx cut (n - 1)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (min_partitions ("aab"))
printfn "%d" (min_partitions ("aaa"))
printfn "%d" (min_partitions ("ababbbabbababa"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
