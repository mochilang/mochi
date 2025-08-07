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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let rec permute (k: int) (arr: int array) (res: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable k = k
    let mutable arr = arr
    let mutable res = res
    try
        if k = 1 then
            let copy: int array = Array.sub arr 0 ((int (Array.length (arr))) - 0)
            __ret <- Array.append res [|copy|]
            raise Return
        res <- permute (k - 1) (arr) (res)
        let mutable i: int = 0
        while i < (k - 1) do
            if (((k % 2 + 2) % 2)) = 0 then
                let temp: int = _idx arr (i)
                arr.[i] <- _idx arr (k - 1)
                arr.[k - 1] <- temp
            else
                let temp: int = _idx arr (0)
                arr.[0] <- _idx arr (k - 1)
                arr.[k - 1] <- temp
            res <- permute (k - 1) (arr) (res)
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and heaps (arr: int array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable arr = arr
    try
        if (Seq.length (arr)) <= 1 then
            __ret <- [|Array.sub arr 0 ((int (Array.length (arr))) - 0)|]
            raise Return
        let mutable res: int array array = Array.empty<int array>
        res <- permute (Seq.length (arr)) (arr) (res)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let perms: int array array = heaps (unbox<int array> [|1; 2; 3|])
        printfn "%s" (_repr (perms))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
