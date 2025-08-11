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
let mutable _seed: int = 1
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        _seed <- int ((((((int64 _seed) * (int64 1103515245)) + (int64 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- _seed
        raise Return
        __ret
    with
        | Return -> __ret
let rec rand_range (max: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable max = max
    try
        __ret <- (((rand()) % max + max) % max)
        raise Return
        __ret
    with
        | Return -> __ret
let rec shuffle (list_int: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable list_int = list_int
    try
        let mutable i: int = (Seq.length (list_int)) - 1
        while i > 0 do
            let j: int = rand_range (i + 1)
            let tmp: int = _idx list_int (int i)
            list_int.[int i] <- _idx list_int (int j)
            list_int.[int j] <- tmp
            i <- i - 1
        __ret <- list_int
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_sorted (list_int: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable list_int = list_int
    try
        let mutable i: int = 0
        while i < ((Seq.length (list_int)) - 1) do
            if (_idx list_int (int i)) > (_idx list_int (int (i + 1))) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec bogo_sort (list_int: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable list_int = list_int
    try
        let mutable res: int array = list_int
        while not (is_sorted (res)) do
            res <- shuffle (res)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let data: int array = unbox<int array> [|3; 2; 1|]
printfn "%s" (_str (bogo_sort (data)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
