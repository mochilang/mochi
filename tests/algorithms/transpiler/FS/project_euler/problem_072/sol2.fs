// Generated 2025-08-12 13:41 +0700

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let rec solution (limit: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable limit = limit
    try
        let mutable phi: int array = Array.empty<int>
        let mutable i: int = 0
        while i <= limit do
            phi <- Array.append phi [|i|]
            i <- i + 1
        let mutable n: int = 2
        while n <= limit do
            if (_idx phi (int n)) = n then
                let mutable k: int = n
                while k <= limit do
                    phi.[k] <- (_idx phi (int k)) - (_floordiv (_idx phi (int k)) n)
                    k <- k + n
            n <- n + 1
        let mutable total: int = 0
        let mutable m: int = 2
        while m <= limit do
            total <- total + (_idx phi (int m))
            m <- m + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (printfn "%d" (solution (8)))
        ignore (printfn "%d" (solution (1000)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
